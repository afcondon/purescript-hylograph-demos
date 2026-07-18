-- | The headless smoke test — M1's exit criterion. Node-run, no
-- | browser: parser goldens, the fresh-start world, the touch cascade,
-- | the braid, explain/axiomsBehind, and one pinned primary
-- | justification (the Engine docstring's "clients pin their feeds").
module Test.JtmsMake.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Jtms.Explain (axiomsBehind, explain)
import Jtms.Kernel (Why(..), factFor, isKnown)
import Make.Model (Path(..), touch)
import Make.Rules (Ax(..), BuildClaim(..), BuildRule(..), buildKB, consistent, staleSet)
import Make.Scenarios (Scenario(..), loadScenario)
import Parsing (parseErrorMessage)

assertTrue :: String -> Boolean -> Effect Unit
assertTrue label ok =
  if ok then log ("  ok: " <> label)
  else throw ("FAILED: " <> label)

main :: Effect Unit
main = do
  log "jtms-make smoke test"
  buildChain
  ecosystem
  log "all green"

buildChain :: Effect Unit
buildChain = case loadScenario BuildChain of
  Left err -> throw ("BuildChain parse failed: " <> parseErrorMessage err)
  Right { model, snapshot } -> do
    log "BuildChain:"
    -- 1. parser goldens (catch expansion regressions)
    assertTrue "12 edges after $(SRCS)/$(STYLE) expansion" (Array.length model.edges == 12)
    assertTrue "5 file targets" (Array.length model.fileTargets == 5)
    assertTrue "6 sources" (Array.length model.sources == 6)
    assertTrue "site is the only phony" (model.phonies == [ Path "site" ])

    -- 2. the just-built world
    let kb0 = buildKB model snapshot
    assertTrue "fresh start: nothing stale" (Set.isEmpty (staleSet kb0))
    assertTrue "fresh start: every file target fresh"
      (Array.all (\t -> isKnown (Fresh t) kb0) model.fileTargets)
    assertTrue "fresh start: consistent" (consistent kb0)

    -- 3. touch a source: exactly its transitive dependents go stale
    let kb1 = buildKB model (touch (Path "src/Rules.purs") snapshot)
    let
      expected = Set.fromFoldable
        (Path <$> [ "output/index.js", "public/bundle.js", "docs/demo/bundle.js" ])
    assertTrue "touch src/Rules.purs: exactly its dependents stale" (staleSet kb1 == expected)
    assertTrue "touch src/Rules.purs: public/index.html untouched"
      (isKnown (Fresh (Path "public/index.html")) kb1)
    assertTrue "touch src/Rules.purs: site will run" (isKnown (WillRun (Path "site")) kb1)
    assertTrue "touch src/Rules.purs: consistent" (consistent kb1)

    -- 4. the braid: shared/style.css feeds both html targets
    let kb2 = buildKB model (touch (Path "shared/style.css") snapshot)
    let braid = Set.fromFoldable (Path <$> [ "public/index.html", "docs/demo/index.html" ])
    assertTrue "touch shared/style.css: both html targets stale" (staleSet kb2 == braid)

    -- 5. provenance reaches the gesture
    assertTrue "explain (Stale docs/demo/bundle.js) exists"
      (isJust (explain (Stale (Path "docs/demo/bundle.js")) kb1))
    let axs = axiomsBehind (Stale (Path "docs/demo/bundle.js")) kb1
    assertTrue "…and rests on the Touched axiom"
      ( Array.any
          ( \f -> case f.why of
              Axiom (Touched p) -> p == Path "src/Rules.purs"
              _ -> false
          )
          axs
      )

    -- 6. golden pin: the primary justification of the first stale link
    assertTrue "golden: Stale output/index.js primarily by StaleNewerDep"
      ( case factFor (Stale (Path "output/index.js")) kb1 of
          Just f -> case f.why of
            ByRule r -> r.rule == StaleNewerDep
            Axiom _ -> false
          Nothing -> false
      )

ecosystem :: Effect Unit
ecosystem = case loadScenario Ecosystem of
  Left err -> throw ("Ecosystem parse failed: " <> parseErrorMessage err)
  Right { model, snapshot } -> do
    log "Ecosystem:"
    assertTrue "the one real dep edge: all <- polyglot"
      (Array.elem { target: Path "all", prereq: Path "polyglot" } model.edges)
    assertTrue "at least 15 declared phonies" (Array.length model.phonies >= 15)

    -- the honest finding: rule targets never declared .PHONY read as
    -- missing files that make will always rebuild
    let undeclared = Set.fromFoldable model.fileTargets
    let
      expected = Set.fromFoldable
        (Path <$> [ "api-index", "docker-libs", "docker-showcases", "serve-blog", "serve-website" ])
    assertTrue "undeclared phonies surface as file targets" (undeclared == expected)

    let kb = buildKB model snapshot
    assertTrue "…and all five are stale (make would always run them)"
      (staleSet kb == expected)
    assertTrue "consistent" (consistent kb)
