-- | Projection of the KB into Sankey inputs. The edges are scanned
-- | from the KB's `Dep` facts — the diagram is a projection of the
-- | belief store, not of the AST.
-- |
-- | Link value = transitive leaf-source mass of the prerequisite
-- | (union, not sum: a shared dep counts once downstream, which is
-- | what makes the braid read). No aggregating value truly flows
-- | through a build system the way money or energy does — leaf-source
-- | mass is the placeholder until a grammar-of-graphics pass decides
-- | what the display value should carry. The `max 1.0` floor keeps
-- | phony-only edges (zero file leaves) visible.
module Make.ToSankey
  ( sankeyLinks
  , leafMass
  , ancestryMap
  , isolatedPaths
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import DataViz.Layout.Sankey.Types (LinkCSVRow)
import Jtms.Explain (explain)
import Jtms.Kernel (knownClaims)
import Make.Model (BuildModel, Path, depsOf, unPath)
import Make.Rules (BuildClaim(..), BuildKB, governing)

-- | Each leaf source carries one unit; a path's mass is the set of
-- | leaves that transitively feed it. One bottom-up fold in topo order.
leafMass :: BuildModel -> Map Path (Set Path)
leafMass model = Array.foldl step Map.empty model.topo
  where
  step acc p = case depsOf model p of
    [] -> Map.insert p (Set.singleton p) acc
    deps -> Map.insert p
      (Set.unions (deps <#> \d -> fromMaybe Set.empty (Map.lookup d acc)))
      acc

-- | The KB's `Dep` facts as layout rows: prereq -> target, mass flows
-- | from sources toward final targets.
sankeyLinks :: BuildModel -> BuildKB -> Array LinkCSVRow
sankeyLinks model kb =
  Array.nub (Array.mapMaybe depLink (Set.toUnfoldable (knownClaims kb)))
  where
  mass = leafMass model

  massOf p = Set.size (fromMaybe Set.empty (Map.lookup p mass))

  depLink = case _ of
    Dep t p -> Just
      { s: unPath p
      , t: unPath t
      , v: max 1.0 (Int.toNumber (massOf p))
      }
    _ -> Nothing

-- | Paths that touch no dep edge at all — Ecosystem's phony
-- | constellation. The Sankey layout only knows nodes that links
-- | mention, so these render as a shelf below the diagram.
isolatedPaths :: BuildModel -> Array Path
isolatedPaths model =
  Array.filter (\p -> not (Set.member p connected))
    (model.sources <> model.fileTargets <> model.phonies)
  where
  connected = Set.fromFoldable
    (Array.concatMap (\e -> [ e.prereq, e.target ]) model.edges)

-- | Per node name, the set of node names in its justification
-- | ancestry: everything mentioned by the derivation DAG behind the
-- | node's governing claim. Drives hover highlighting — hovering a
-- | target lights the story of why it is what it is.
ancestryMap :: BuildModel -> BuildKB -> Map String (Set String)
ancestryMap model kb =
  Map.fromFoldable
    ( (model.sources <> model.fileTargets <> model.phonies) <#> \p ->
        Tuple (unPath p) (ancestryOf p)
    )
  where
  ancestryOf p = case governing kb p >>= \c -> explain c kb of
    Nothing -> Set.singleton (unPath p)
    Just dag -> Set.insert (unPath p)
      (Set.fromFoldable (Array.concatMap (pathsIn <<< _.claim) dag.nodes))

  pathsIn = map unPath <<< case _ of
    Dep t p -> [ t, p ]
    HasRecipe p -> [ p ]
    Phony p -> [ p ]
    SourceLeaf p -> [ p ]
    Exists p -> [ p ]
    Missing p -> [ p ]
    MTime p _ -> [ p ]
    Newer d t -> [ d, t ]
    Stale p -> [ p ]
    Fresh p -> [ p ]
    WillRun p -> [ p ]
