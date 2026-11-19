-- | Force layout for the Honeycomb Puzzle.
-- |
-- | Each of the six graphs has a simulation of its own: a
-- | `Hylograph.Simulation.Stepped` handle on the default (pure PureScript)
-- | engine, stepped by the page's one animation loop. The handle copies the
-- | nodes it is given and hands back new records every tick, so six handles
-- | made from the same starting circle are six independent layouts.
module Honeycomb.Simulation
  ( HoneycombNode
  , NodePositions
  , Layout
  , createLayout
  , relinkLayout
  , stepLayout
  , honeycombSetup
  , frame
  , nodeRadius
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.ForceEngine.Setup (Region(..), Setup)
import Hylograph.ForceEngine.Setup as Setup
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.Simulation.Stepped (Engine(..), Stepped, Tolerance)
import Hylograph.Simulation.Stepped as Stepped

import Honeycomb.Math (cos, sin, pi)
import Honeycomb.Types (NodeId(..), EdgeKey, allNodes)

-- =============================================================================
-- Types
-- =============================================================================

-- | A node of a Honeycomb simulation.
type HoneycombNode = SimulationNode (nodeId :: NodeId)

-- | Node positions keyed by NodeId.
type NodePositions = Map NodeId { x :: Number, y :: Number }

-- | One graph's layout: its simulation, where its nodes are now, and whether
-- | it is still moving (a graph at rest is not stepped).
type Layout =
  { simulation :: Stepped (nodeId :: NodeId)
  , positions :: NodePositions
  , moving :: Boolean
  }

-- =============================================================================
-- The frame every graph is drawn in
-- =============================================================================

-- | The SVG viewBox each graph is drawn in (see `Honeycomb.Render`).
frame :: { width :: Number, height :: Number }
frame = { width: 180.0, height: 150.0 }

-- | The radius every node is drawn with.
nodeRadius :: Number
nodeRadius = 12.0

-- | Half the width of the stroke round each node: what is kept inside the
-- | frame is the circle as drawn, stroke and all.
halfStroke :: Number
halfStroke = 1.0

-- =============================================================================
-- Initialization
-- =============================================================================

-- | The nodes where every graph starts: a circle centred in the frame.
initialNodes :: Array HoneycombNode
initialNodes = Array.mapWithIndex node allNodes
  where
  node index nodeId =
    let
      angle = toNumber index * 2.0 * pi / 8.0 - pi / 2.0
      radius = 45.0
    in
      { id: nodeIdToIndex nodeId
      , nodeId
      , x: frame.width / 2.0 + radius * cos angle
      , y: frame.height / 2.0 + radius * sin angle
      , vx: 0.0
      , vy: 0.0
      , fx: null
      , fy: null
      }

-- | A layout of the eight nodes joined by these edges, starting from the
-- | circle, moving.
createLayout :: Set EdgeKey -> Effect Layout
createLayout edges = do
  simulation <- Stepped.create Default honeycombSetup initialNodes (edgesToLinks edges)
  nodes <- simulation.nodes
  pure { simulation, positions: positionsOf nodes, moving: true }

-- | The same layout with these edges. If they are not the edges it has, it
-- | is relinked and reheated, so it moves to fit them; otherwise it is left
-- | as it is.
relinkLayout :: Set EdgeKey -> Layout -> Effect Layout
relinkLayout edges layout = do
  let links = edgesToLinks edges
  current <- layout.simulation.links
  if Set.fromFoldable (linkPair <$> current) == Set.fromFoldable (linkPair <$> links) then
    pure layout
  else do
    layout.simulation.relink links
    layout.simulation.reheat
    pure layout { moving = true }
  where
  linkPair link = Tuple link.source link.target

-- | One tick of a layout that is moving; a layout at rest is left alone.
stepLayout :: Layout -> Effect Layout
stepLayout layout
  | not layout.moving = pure layout
  | otherwise = do
      nodes <- layout.simulation.step
      cooled <- layout.simulation.cooled
      settled <- layout.simulation.settled atRest
      pure layout { positions = positionsOf nodes, moving = not (cooled || settled) }

-- | Still enough to stop stepping: no node moves a hundredth of a unit in a
-- | tick (the frame is 180 units across, drawn about 200 pixels wide).
atRest :: Tolerance
atRest = { step: 0.01, acceleration: 0.01 }

-- =============================================================================
-- Forces
-- =============================================================================

-- | Pull toward the middle of the frame, push the nodes apart, hold joined
-- | nodes at a distance, and keep every node, as drawn, inside the frame.
honeycombSetup :: Setup HoneycombNode
honeycombSetup = Setup.setupWithParams "honeycomb"
  [ Setup.positionX "centerX"
      # Setup.withX (Setup.static (frame.width / 2.0))
      # Setup.withStrength (Setup.static 0.08)
  , Setup.positionY "centerY"
      # Setup.withY (Setup.static (frame.height / 2.0))
      # Setup.withStrength (Setup.static 0.08)
  , Setup.manyBody "charge"
      # Setup.withStrength (Setup.static (-120.0))
  , Setup.link "links"
      # Setup.withDistance (Setup.static 35.0)
      # Setup.withStrength (Setup.static 0.3)
  , Setup.contain "frame" (RectRegion { x0: 0.0, y0: 0.0, x1: frame.width, y1: frame.height })
      # Setup.withRadius (Setup.static (nodeRadius + halfStroke))
  ]
  -- d3's defaults, which the demo was tuned with: alpha decays to alphaMin
  -- in 300 ticks, and a node keeps 60% of its velocity from tick to tick.
  (Setup.defaultParams { alphaDecay = 0.0228, velocityRetention = 0.6 })

-- =============================================================================
-- Edges as links
-- =============================================================================

-- | Edges as links between node ids.
edgesToLinks :: Set EdgeKey -> Array { source :: Int, target :: Int }
edgesToLinks edges = Set.toUnfoldable edges <#> \edge ->
  { source: nodeIdToIndex edge.a, target: nodeIdToIndex edge.b }

-- | A node's id in the simulation: its place in `allNodes`.
nodeIdToIndex :: NodeId -> Int
nodeIdToIndex = case _ of
  C -> 0
  G -> 1
  D -> 2
  A -> 3
  E -> 4
  B -> 5
  FSharp -> 6
  CSharp -> 7

positionsOf :: Array HoneycombNode -> NodePositions
positionsOf nodes = Map.fromFoldable $ nodes <#> \node -> Tuple node.nodeId { x: node.x, y: node.y }
