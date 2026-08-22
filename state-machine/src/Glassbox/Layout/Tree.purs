-- | Glassbox.Layout.Tree
-- |
-- | Position states as the tree the user experiences, not as a ring.
-- |
-- | This is the join the spec called for: `Data.Graph.Layout.treeLayout` in
-- | hylograph-graph knows how to lay out a rooted tree tidily and had no state
-- | machine to lay out; `DataViz.Layout.StateMachine` knows how to draw a state
-- | machine and shipped only `circularLayout` and `gridLayout`. The latter's
-- | `layoutWithConfig` takes the positioning pass as an argument, which is
-- | exactly the extension point needed — so this is a third strategy rather than
-- | a fork of anything.
-- |
-- | Unreachable states are parked in a band below the tree instead of being
-- | dropped. A state home cannot reach is a finding, and a layout that silently
-- | omitted it would be hiding the most useful thing on the page.
module Glassbox.Layout.Tree
  ( treeStrategy
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Graph.Layout as GL
import DataViz.Layout.StateMachine (LayoutConfig, LayoutState, State)
import Glassbox.Tree (Induced)

treeStrategy :: forall extra. Induced -> LayoutConfig -> Array (State extra) -> Array (LayoutState extra)
treeStrategy induced config states = map place states
  where
  placed :: Array (GL.LayoutNode String)
  placed = GL.treeLayout graphConfig induced.root induced.children

  graphConfig =
    -- nodeWidth spaces siblings *within* a layer, nodeHeight spaces the layers
    -- apart; under Horizontal those map to vertical and horizontal respectively.
    { nodeWidth: 2.0 * config.stateRadiusY + 110.0
    , nodeHeight: 2.0 * config.stateRadiusX + 90.0
    -- Horizontal, so depth reads left-to-right: distance from home is distance
    -- across the page. A six-state chain laid out vertically is a column
    -- taller than the pane, which wastes the one axis a landscape pane has.
    , orientation: GL.Horizontal
    , reversed: false
    }

  positions :: Map String { x :: Number, y :: Number }
  positions = Map.fromFoldable (map (\n -> Tuple n.id { x: n.x, y: n.y }) placed)

  -- Shift the whole tree so nothing sits on or past the left/top edge.
  bounds = foldl widen { minX: 0.0, minY: 0.0, maxY: 0.0 } placed
    where
    widen acc n =
      { minX: min acc.minX n.x, minY: min acc.minY n.y, maxY: max acc.maxY n.y }

  offsetX = config.margin - bounds.minX
  offsetY = config.margin - bounds.minY

  -- Whatever the root cannot reach goes in a band underneath, in a row.
  parked :: Map String { x :: Number, y :: Number }
  parked = Map.fromFoldable (Array.mapWithIndex park induced.unreachable)
    where
    park i id = Tuple id
      { x: bounds.minX + toNumber i * (2.0 * config.stateRadiusX + 45.0)
      , y: bounds.maxY + 2.0 * config.stateRadiusY + 95.0
      }

  place state =
    let
      point = fromMaybe { x: 0.0, y: 0.0 } $
        case Map.lookup state.id positions of
          Just p -> Just p
          Nothing -> Map.lookup state.id parked
    in
      { state
      , position:
          { cx: point.x + offsetX
          , cy: point.y + offsetY
          , rx: config.stateRadiusX
          , ry: config.stateRadiusY
          }
      }
