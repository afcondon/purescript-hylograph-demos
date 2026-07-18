-- | Make's semantics as monotone JTMS rules: staleness and
-- | up-to-dateness are derived beliefs with provenance.
-- |
-- | Soundness by construction — ZERO `absent` in this rule set. A
-- | negation-as-failure misfire (`Fresh` minted before `Stale` lands a
-- | pass later) would be unretractable in a monotone KB. Instead every
-- | would-be negation is a positive fact the snapshot's totality
-- | supplies: each path is axiomatically `Exists` or `Missing`, every
-- | existing path has an `MTime`, and `SourceLeaf` vs `HasRecipe` is
-- | decided at parse time. The result is positive Datalog with
-- | arithmetic guards: monotone, confluent, saturation-order
-- | independent in its claim set. Rule order only chooses which
-- | justification becomes a multiply-derivable fact's primary `why`
-- | (the rest land in `alsoWhy` — "stale three ways").
-- |
-- | "Touch" is not retraction: a new snapshot is a new axiom set, and
-- | the KB is rebuilt from scratch and re-saturated — cheap at
-- | Makefile scale.
module Make.Rules
  ( BuildClaim(..)
  , BuildRule(..)
  , Ax(..)
  , BuildKB
  , TargetState(..)
  , engine
  , axioms
  , buildKB
  , governing
  , stateOf
  , staleSet
  , freshSet
  , willRunSet
  , consistent
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (foldl, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Jtms.Engine (Engine)
import Jtms.Engine (saturate) as Engine
import Jtms.Kernel (KB, assertAxiom, emptyKB, isKnown, knownClaims)
import Jtms.Rule (RuleM, each, matches, require, rule)
import Make.Model (BuildModel, Path, Snapshot, depsOf, filePaths, unPath)

data BuildClaim
  -- from the Makefile
  = Dep Path Path -- Dep target prereq: the edge
  | HasRecipe Path
  | Phony Path
  | SourceLeaf Path -- referenced by some rule, built by none
  -- from the snapshot
  | Exists Path
  | Missing Path -- positive complement — asserted, never inferred by absence
  | MTime Path Int
  -- derived
  | Newer Path Path -- Newer dep target: mtime dep > mtime target
  | Stale Path -- make would rebuild this file target
  | Fresh Path -- make would leave it alone
  | WillRun Path -- the recipe executes on `make`

derive instance Eq BuildClaim
derive instance Ord BuildClaim

instance Show BuildClaim where
  show = case _ of
    Dep t p -> "Dep " <> unPath t <> " <- " <> unPath p
    HasRecipe p -> "HasRecipe " <> unPath p
    Phony p -> "Phony " <> unPath p
    SourceLeaf p -> "SourceLeaf " <> unPath p
    Exists p -> "Exists " <> unPath p
    Missing p -> "Missing " <> unPath p
    MTime p t -> "MTime " <> unPath p <> " = " <> show t
    Newer d t -> "Newer " <> unPath d <> " > " <> unPath t
    Stale p -> "Stale " <> unPath p
    Fresh p -> "Fresh " <> unPath p
    WillRun p -> "WillRun " <> unPath p

data BuildRule
  = SourceFresh -- an existing source file is fresh
  | DepNewer -- mtime(dep) > mtime(target), citing both MTime facts
  | StaleMissing -- buildable target whose file is absent
  | StaleNewerDep -- some prerequisite is newer
  | StaleCascade -- some prerequisite is itself stale
  | StalePhonyDep -- a phony prereq always runs, so the target always rebuilds
  | AllDepsFresh -- exists, every dep fresh and not newer
  | PhonyAlwaysRuns -- .PHONY targets run unconditionally
  | Rebuilds -- stale + has a recipe => the recipe runs

derive instance Eq BuildRule

instance Show BuildRule where
  show = case _ of
    SourceFresh -> "SourceFresh"
    DepNewer -> "DepNewer"
    StaleMissing -> "StaleMissing"
    StaleNewerDep -> "StaleNewerDep"
    StaleCascade -> "StaleCascade"
    StalePhonyDep -> "StalePhonyDep"
    AllDepsFresh -> "AllDepsFresh"
    PhonyAlwaysRuns -> "PhonyAlwaysRuns"
    Rebuilds -> "Rebuilds"

-- | Axiom sources. `Touched p` stamps the bumped mtime after a click,
-- | so the proof panel can say "…because you touched src/Foo.purs".
data Ax = FromMakefile | FromSnapshot | Touched Path

derive instance Eq Ax

instance Show Ax where
  show = case _ of
    FromMakefile -> "the Makefile"
    FromSnapshot -> "the filesystem"
    Touched p -> "touching " <> unPath p

type BuildKB = KB Unit BuildClaim BuildRule Ax

-- | Cite a path's MTime fact and yield its timestamp.
mtimeOf :: forall res. Path -> RuleM res BuildClaim BuildRule Ax Int
mtimeOf p = matches case _ of
  MTime q t | q == p -> Just t
  _ -> Nothing

-- | The engine closes over the static model, exactly as the Sudoku
-- | client's rules close over its cells and houses.
engine :: BuildModel -> Engine Unit BuildClaim BuildRule Ax
engine model =
  { rules:
      [ rule SourceFresh do
          p <- each model.sources
          require (Exists p)
          pure (Fresh p)

      , rule DepNewer do
          e <- each model.edges
          dt <- mtimeOf e.prereq
          tt <- mtimeOf e.target
          guard (dt > tt)
          pure (Newer e.prereq e.target)

      , rule StaleMissing do
          t <- each model.fileTargets
          require (HasRecipe t)
          require (Missing t)
          pure (Stale t)

      , rule StaleNewerDep do
          e <- each fileTargetEdges
          require (Newer e.prereq e.target)
          pure (Stale e.target)

      , rule StaleCascade do
          e <- each fileTargetEdges
          require (Stale e.prereq)
          pure (Stale e.target)

      , rule StalePhonyDep do
          e <- each fileTargetEdges
          require (Phony e.prereq)
          pure (Stale e.target)

      , rule AllDepsFresh do
          t <- each model.fileTargets
          require (Exists t)
          tt <- mtimeOf t
          traverse_
            ( \d -> do
                require (Fresh d)
                dt <- mtimeOf d
                guard (tt >= dt)
            )
            (depsOf model t)
          pure (Fresh t)

      , rule PhonyAlwaysRuns do
          t <- each model.phonies
          require (Phony t)
          pure (WillRun t)

      , rule Rebuilds do
          t <- each model.fileTargets
          require (Stale t)
          require (HasRecipe t)
          pure (WillRun t)
      ]
  , refine: \_ res -> res
  }
  where
  fileTargetSet = Set.fromFoldable model.fileTargets
  fileTargetEdges = Array.filter (\e -> Set.member e.target fileTargetSet) model.edges

-- | The axiom set: Makefile shape tagged `FromMakefile`, filesystem
-- | state tagged `FromSnapshot` — except the just-touched path's
-- | mtime, which carries `Touched` for the proof narrative.
axioms :: BuildModel -> Snapshot -> Array { source :: Ax, claim :: BuildClaim }
axioms model snap = makefileAxioms <> snapshotAxioms
  where
  makefileAxioms = { source: FromMakefile, claim: _ } <$>
    ( (model.edges <#> \e -> Dep e.target e.prereq)
        <> (Array.fromFoldable (Map.keys model.recipes) <#> HasRecipe)
        <> (model.phonies <#> Phony)
        <> (model.sources <#> SourceLeaf)
    )

  snapshotAxioms = filePaths model >>= \p ->
    case Map.lookup p snap.mtimes of
      Just t | not (Set.member p snap.missing) ->
        [ { source: FromSnapshot, claim: Exists p }
        , { source: mtimeTag p, claim: MTime p t }
        ]
      _ -> [ { source: FromSnapshot, claim: Missing p } ]

  mtimeTag p = case snap.lastTouched of
    Just q | q == p -> Touched p
    _ -> FromSnapshot

-- | Assert the axioms, saturate. Derived facts carry the snapshot's
-- | gesture as their source: `Touched p` after a click.
buildKB :: BuildModel -> Snapshot -> BuildKB
buildKB model snap =
  Engine.saturate (engine model) derivedSource seeded
  where
  derivedSource = maybe FromSnapshot Touched snap.lastTouched
  seeded = foldl (\kb ax -> assertAxiom ax.source ax.claim kb) (emptyKB unit) (axioms model snap)

-- | Render-facing summary of what the KB believes about a path.
data TargetState
  = SourceState
  | FreshState
  | StaleState
  | MissingState
  | PhonyState
  | UnknownState

derive instance Eq TargetState

-- | The claim that best summarises what the KB holds about a path —
-- | the root the proof panel and hover ancestry explain from.
governing :: BuildKB -> Path -> Maybe BuildClaim
governing kb p =
  if isKnown (Stale p) kb then Just (Stale p)
  else if isKnown (Fresh p) kb then Just (Fresh p)
  else if isKnown (WillRun p) kb then Just (WillRun p)
  else if isKnown (Missing p) kb then Just (Missing p)
  else Nothing

stateOf :: BuildKB -> Path -> TargetState
stateOf kb p
  | isKnown (Phony p) kb = PhonyState
  | isKnown (Missing p) kb = MissingState
  | isKnown (Stale p) kb = StaleState
  | isKnown (SourceLeaf p) kb && isKnown (Fresh p) kb = SourceState
  | isKnown (Fresh p) kb = FreshState
  | otherwise = UnknownState

selectClaims :: (BuildClaim -> Maybe Path) -> BuildKB -> Set Path
selectClaims sel kb =
  Set.fromFoldable (Array.mapMaybe sel (Set.toUnfoldable (knownClaims kb)))

staleSet :: BuildKB -> Set Path
staleSet = selectClaims case _ of
  Stale p -> Just p
  _ -> Nothing

freshSet :: BuildKB -> Set Path
freshSet = selectClaims case _ of
  Fresh p -> Just p
  _ -> Nothing

willRunSet :: BuildKB -> Set Path
willRunSet = selectClaims case _ of
  WillRun p -> Just p
  _ -> Nothing

-- | The invariant NAF-freedom buys: no path is both fresh and stale.
-- | Violated only by a malformed snapshot (garbage in).
consistent :: BuildKB -> Boolean
consistent kb = Set.isEmpty (Set.intersection (staleSet kb) (freshSet kb))
