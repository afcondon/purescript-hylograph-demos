-- | Prose for the proof card: the derivation DAG behind a path's
-- | governing claim, one line per fact in feed order (Explain
-- | guarantees premises before conclusions), with an "ultimately
-- | because" footer that leads with the touch gesture when there is
-- | one.
module Make.Proof
  ( headline
  , proofLines
  , ultimately
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Jtms.Explain (axiomsBehind, explain)
import Jtms.Kernel (Fact, Why(..))
import Make.Model (Path, unPath)
import Make.Rules (Ax(..), BuildClaim, BuildKB, BuildRule(..), TargetState(..), governing, stateOf)

-- | One word for the card title.
headline :: BuildKB -> Path -> String
headline kb p = case stateOf kb p of
  SourceState -> "source, present"
  FreshState -> "fresh — make would leave it alone"
  StaleState -> "stale — make would rebuild it"
  MissingState -> "missing — make would build it"
  PhonyState -> "phony — runs every time"
  UnknownState -> "unknown to the model"

-- | The derivation, one line per fact.
proofLines :: BuildKB -> Path -> Array String
proofLines kb p = case governing kb p >>= \c -> explain c kb of
  Nothing -> []
  Just dag -> describeFact <$> dag.nodes

describeFact :: Fact BuildClaim BuildRule Ax -> String
describeFact f = case f.why of
  Axiom ax -> show f.claim <> " — axiom, from " <> show ax
  ByRule r ->
    show f.claim <> " — " <> ruleProse r.rule <> alsoCount
  where
  alsoCount = case Array.length f.alsoWhy of
    0 -> ""
    n -> " (+" <> show n <> " more " <> (if n == 1 then "derivation" else "derivations") <> ")"

ruleProse :: BuildRule -> String
ruleProse = case _ of
  SourceFresh -> "a source file that exists is fresh"
  DepNewer -> "the prerequisite's mtime beats the target's"
  StaleMissing -> "the file doesn't exist, and make can build it"
  StaleNewerDep -> "a prerequisite is newer than the target"
  StaleCascade -> "a prerequisite will itself be rebuilt"
  StalePhonyDep -> "a phony prerequisite always runs, so this always rebuilds"
  AllDepsFresh -> "it exists, and every prerequisite is fresh and no newer"
  PhonyAlwaysRuns -> ".PHONY targets run unconditionally"
  Rebuilds -> "it is stale and has a recipe"

-- | "Ultimately because …" — the axioms the belief rests on, leading
-- | with the touch gesture if one is among them.
ultimately :: BuildKB -> Path -> Maybe String
ultimately kb p = case governing kb p of
  Nothing -> Nothing
  Just claim ->
    let
      axs = axiomsBehind claim kb
      touched = Array.findMap
        ( \f -> case f.why of
            Axiom (Touched q) -> Just (unPath q)
            _ -> Nothing
        )
        axs
    in
      case touched of
        Just q -> Just ("ultimately because you touched " <> q)
        Nothing ->
          if Array.null axs then Nothing
          else Just ("rests on " <> show (Array.length axs) <> " axioms from the Makefile and the filesystem")
