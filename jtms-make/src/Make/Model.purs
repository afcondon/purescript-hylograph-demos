-- | The static build model extracted from a parsed Makefile, plus the
-- | simulated filesystem snapshot the rules read. Pattern rules and
-- | dot-special targets are filtered; variables in target/prereq
-- | position are expanded and word-split; order-only prerequisites are
-- | folded into normal ones (ordering vs content doesn't matter to a
-- | static read).
module Make.Model
  ( Path(..)
  , unPath
  , Edge
  , Snapshot
  , BuildModel
  , fromMakefile
  , depsOf
  , filePaths
  , touch
  , allFresh
  , sourcesOnly
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (fromWeightedDigraph, topologicalSort)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Make.Ast (Makefile)
import Make.Expand (expandNames, render)

newtype Path = Path String

derive instance Eq Path
derive instance Ord Path
derive newtype instance Show Path

unPath :: Path -> String
unPath (Path p) = p

-- | One prerequisite edge: `target: prereq`.
type Edge = { target :: Path, prereq :: Path }

-- | The simulated filesystem. Total over the model's file paths: a
-- | path is either in `mtimes` (exists, with a timestamp) or in
-- | `missing` — this totality is what lets the rules stay positive
-- | (no negation-as-failure anywhere).
type Snapshot =
  { mtimes :: Map Path Int
  , missing :: Set Path
  , clock :: Int
  , lastTouched :: Maybe Path
  }

type BuildModel =
  { edges :: Array Edge
  , fileTargets :: Array Path -- rule targets not declared .PHONY
  , phonies :: Array Path
  , sources :: Array Path -- referenced as prereq, built by nothing
  , recipes :: Map Path (Array String) -- rendered recipe text, for the proof panel
  , docs :: Map Path String -- ## doc comments, for captions
  , topo :: Array Path -- dependencies before dependents
  }

fromMakefile :: Makefile -> BuildModel
fromMakefile mf =
  { edges, fileTargets, phonies, sources, recipes, docs, topo }
  where
  expanded = Array.filter (not <<< _.isPatternRule) mf.rules <#> \r ->
    { targets:
        Array.filter (not <<< special)
          (Path <$> Array.concatMap (expandNames mf.variables <<< _.name) r.targets)
    , prereqs:
        Path <$> Array.concatMap (expandNames mf.variables)
          (r.prerequisites.normal <> r.prerequisites.orderOnly)
    , commands: maybe [] (\rec -> render <<< _.text <$> rec.commands) r.recipe
    , hasRecipe: case r.recipe of
        Just _ -> true
        Nothing -> false
    , doc: r.docComment
    }

  special (Path name) = Set.member name dotSpecials

  dotSpecials = Set.fromFoldable
    [ ".PHONY"
    , ".ONESHELL"
    , ".SUFFIXES"
    , ".DEFAULT"
    , ".PRECIOUS"
    , ".SILENT"
    , ".SECONDARY"
    , ".INTERMEDIATE"
    , ".DELETE_ON_ERROR"
    , ".EXPORT_ALL_VARIABLES"
    , ".NOTPARALLEL"
    ]

  edges = Array.nub do
    r <- expanded
    t <- r.targets
    p <- r.prereqs
    pure { target: t, prereq: p }

  ruleTargets = Set.fromFoldable (Array.concatMap _.targets expanded)
  phonySet = Set.map Path mf.phonyTargets
  prereqSet = Set.fromFoldable (Array.concatMap _.prereqs expanded)

  phonies = Array.fromFoldable phonySet
  fileTargets = Array.fromFoldable (Set.difference ruleTargets phonySet)
  sources = Array.fromFoldable (Set.difference (Set.difference prereqSet ruleTargets) phonySet)

  recipes = Map.fromFoldable do
    r <- expanded
    guard r.hasRecipe
    t <- r.targets
    pure (Tuple t r.commands)

  docs = Map.fromFoldable do
    r <- expanded
    d <- maybe [] pure r.doc
    t <- r.targets
    pure (Tuple t d)

  topo =
    let
      allNodes = Array.nub
        (Array.concatMap (\e -> [ e.prereq, e.target ]) edges <> sources <> fileTargets <> phonies)
      digraph = fromEdges (edges <#> \e -> { source: e.prereq, target: e.target, weight: unit })
    in
      case fromWeightedDigraph digraph of
        Right dag ->
          let sorted = topologicalSort dag
          in sorted <> Array.filter (\n -> not (Array.elem n sorted)) allNodes
        Left _ -> allNodes -- cyclic Makefile: degrade to declaration order

depsOf :: BuildModel -> Path -> Array Path
depsOf model t = _.prereq <$> Array.filter (\e -> e.target == t) model.edges

-- | Every path that is (or could be) an actual file: sources plus
-- | non-phony targets. Phonies are never files.
filePaths :: BuildModel -> Array Path
filePaths model = model.sources <> model.fileTargets

-- | Bump a path's mtime past everything else and record the gesture.
touch :: Path -> Snapshot -> Snapshot
touch p snap =
  { mtimes: Map.insert p (snap.clock + 1) snap.mtimes
  , missing: Set.delete p snap.missing
  , clock: snap.clock + 1
  , lastTouched: Just p
  }

-- | Everything exists, mtimes ascending in dependency order — the
-- | just-built world, where the first touch tells the whole story.
allFresh :: BuildModel -> Snapshot
allFresh model =
  { mtimes: Map.fromFoldable (Array.mapWithIndex (\i p -> Tuple p (i + 1)) ordered)
  , missing: Set.empty
  , clock: Array.length ordered + 1
  , lastTouched: Nothing
  }
  where
  files = Set.fromFoldable (filePaths model)
  ordered = Array.filter (\p -> Set.member p files) model.topo

-- | Sources exist, targets don't — the fresh-checkout world.
sourcesOnly :: BuildModel -> Snapshot
sourcesOnly model =
  { mtimes: Map.fromFoldable (Array.mapWithIndex (\i p -> Tuple p (i + 1)) model.sources)
  , missing: Set.fromFoldable model.fileTargets
  , clock: Array.length model.sources + 1
  , lastTouched: Nothing
  }
