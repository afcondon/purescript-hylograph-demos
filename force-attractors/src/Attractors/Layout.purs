-- | The attractors: five pure layouts of the same hierarchy, each reduced to
-- | what the simulation needs (a target point for every node) and what the
-- | picture needs (the layout's own geometry, drawn faintly behind the nodes).
-- |
-- | Nothing here knows about the simulation. To add a layout, add a
-- | constructor and a case in `arrange`. The hierarchy is described to the
-- | layout library once (`sized`); `DataViz.Layout.Hierarchy.Placed` runs any
-- | of its layouts on that and answers with a place for every node.
module Attractors.Layout
  ( Layout(..)
  , allLayouts
  , label
  , Point
  , Guide(..)
  , Arrangement
  , arrange
  , stage
  ) where

import Prelude

import Attractors.Flare (Flare(..), NodePath, isLeaf)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number (cos, pi, sin)
import DataViz.Layout.Hierarchy.Cluster as Cluster
import DataViz.Layout.Hierarchy.Pack as Pack
import DataViz.Layout.Hierarchy.Partition as Partition
import DataViz.Layout.Hierarchy.Placed (Extent(..), Hierarchy, Placement)
import DataViz.Layout.Hierarchy.Placed as Placed
import DataViz.Layout.Hierarchy.Tree as Tree
import DataViz.Layout.Hierarchy.Treemap as Treemap

data Layout
  = TidyTree
  | RadialCluster
  | CirclePack
  | Treemap
  | Sunburst

derive instance Eq Layout
derive instance Ord Layout

allLayouts :: Array Layout
allLayouts = [ TidyTree, RadialCluster, CirclePack, Treemap, Sunburst ]

label :: Layout -> String
label = case _ of
  TidyTree -> "Tree"
  RadialCluster -> "Radial cluster"
  CirclePack -> "Circle pack"
  Treemap -> "Treemap"
  Sunburst -> "Sunburst"

type Point = { x :: Number, y :: Number }

-- | A piece of a layout's own geometry.
data Guide
  = Ring { x :: Number, y :: Number, radius :: Number }
  | Box { x :: Number, y :: Number, width :: Number, height :: Number }
  | Outline { path :: String, at :: Point }

type Arrangement =
  { targets :: Map NodePath Point -- where each node should come to rest
  , guides :: Array Guide
  , linked :: Boolean -- whether the layout is read through its parent-child links
  }

-- | The drawing surface every layout is fitted to.
stage :: { width :: Number, height :: Number }
stage = { width: 1200.0, height: 760.0 }

middle :: Point
middle = { x: stage.width / 2.0, y: stage.height / 2.0 }

arrange :: Layout -> Flare -> Arrangement
arrange = case _ of
  TidyTree -> tidyTree
  RadialCluster -> radialCluster
  CirclePack -> circlePack
  Treemap -> treemap
  Sunburst -> sunburst

-- | The hierarchy as the layout library is told about it: once, for all
-- | five layouts. Each layout answers with a `Map` from a node's path to
-- | where it put the node (`Placed.Placement`).
sized :: Flare -> Hierarchy Flare NodePath
sized flare =
  { root: flare
  , children: \(Flare node) -> node.children
  , value: \(Flare node) -> node.value
  , key: \(Flare node) -> node.path
  }

-- | The same hierarchy with every leaf counting the same.
counted :: Flare -> Hierarchy Flare NodePath
counted flare = (sized flare) { value = const 1.0 }

-- | A point at an angle (0 at the top, clockwise) and a distance from the
-- | middle of the stage.
polar :: Number -> Number -> Point
polar angle distance =
  { x: middle.x + distance * cos (angle - pi / 2.0)
  , y: middle.y + distance * sin (angle - pi / 2.0)
  }

-- ============================================================================
-- Tree and cluster: a node is a point, read through its links
-- ============================================================================

tidyTree :: Flare -> Arrangement
tidyTree flare =
  { targets: placed <#> \node -> { x: margin.x + node.x, y: margin.y + node.y }
  , guides: []
  , linked: true
  }
  where
  margin = { x: 30.0, y: 70.0 }
  size = { width: stage.width - 2.0 * margin.x, height: stage.height - 2.0 * margin.y }
  placed = Placed.tree (Tree.defaultTreeConfig { size = size }) (sized flare)

-- | The cluster layout's x is read as an angle and its y as a radius. The
-- | leaves are spread over all but one leaf's share of the circle, so that the
-- | first and the last do not land on each other.
radialCluster :: Flare -> Arrangement
radialCluster flare =
  { targets: placed <#> \node -> polar node.x node.y
  , guides: []
  , linked: true
  }
  where
  radius = 340.0
  leaves = toNumber (leafCount flare)
  size = { width: 2.0 * pi * (leaves - 1.0) / leaves, height: radius }
  placed = Placed.cluster (Cluster.defaultClusterConfig { size = size }) (sized flare)

leafCount :: Flare -> Int
leafCount flare@(Flare node) =
  if isLeaf flare then 1 else sum (leafCount <$> node.children)

-- ============================================================================
-- Circle pack
-- ============================================================================

-- | A leaf rests at the centre of its circle. An internal node's circle is
-- | where its children are, so it rests on the rim: at the top for the root,
-- | an eighth of a turn further round for each level below, which keeps a
-- | parent clear of a child whose circle touches its own.
circlePack :: Flare -> Arrangement
circlePack flare =
  { targets: placed <#> rest
  , guides: Array.fromFoldable placed # Array.mapMaybe \node -> case node.extent of
      Circle circle -> Just (Ring { x: corner.x + node.x, y: corner.y + node.y, radius: circle.radius })
      _ -> Nothing
  , linked: false
  }
  where
  diameter = stage.height - 30.0
  corner = { x: middle.x - diameter / 2.0, y: middle.y - diameter / 2.0 }
  placed = Placed.pack (Pack.defaultPackConfig { size = { width: diameter, height: diameter }, padding = 4.0 }) (sized flare)

  rest :: Placement -> Point
  rest node = case node.extent of
    Circle circle | not node.leaf ->
      { x: corner.x + node.x + circle.radius * sin (toNumber node.depth * pi / 4.0)
      , y: corner.y + node.y - circle.radius * cos (toNumber node.depth * pi / 4.0)
      }
    _ -> { x: corner.x + node.x, y: corner.y + node.y }

-- ============================================================================
-- Treemap
-- ============================================================================

-- | A leaf rests at the middle of its rectangle. An internal node has no area
-- | of its own but the strip its children leave free at the top, so it rests
-- | at the left end of that strip.
treemap :: Flare -> Arrangement
treemap flare =
  { targets: placed <#> rest
  , guides: Array.fromFoldable placed # Array.mapMaybe \node -> case node.extent of
      Rectangle r -> Just (Box { x: margin.x + r.x0, y: margin.y + r.y0, width: r.x1 - r.x0, height: r.y1 - r.y0 })
      _ -> Nothing
  , linked: false
  }
  where
  margin = { x: 30.0, y: 30.0 }
  strip = 16.0
  config = Treemap.defaultTreemapConfig
    { size = { width: stage.width - 2.0 * margin.x, height: stage.height - 2.0 * margin.y }
    , paddingTop = strip
    , paddingInner = 2.0
    , paddingOuter = 2.0
    }
  placed = Placed.treemap config (sized flare)

  rest :: Placement -> Point
  rest node = case node.extent of
    Rectangle r | not node.leaf -> { x: margin.x + r.x0 + strip / 2.0, y: margin.y + r.y0 + strip / 2.0 }
    _ -> { x: margin.x + node.x, y: margin.y + node.y }

-- ============================================================================
-- Sunburst: the partition layout, read as angle and radius
-- ============================================================================

-- | A node rests in the middle of its wedge; the root, whose wedge is the
-- | whole inner disc, in the middle of the stage. Every leaf has an equal
-- | wedge: sized by value, the smallest leaves' wedges are half a pixel wide,
-- | and their nodes would be asked to rest on top of each other.
sunburst :: Flare -> Arrangement
sunburst flare =
  { targets: placed <#> rest
  , guides: Array.fromFoldable placed # Array.mapMaybe \node -> case node.extent of
      Rectangle r -> Just (Outline { path: Partition.sunburstArcPath r.x0 r.y0 r.x1 r.y1 radius, at: middle })
      _ -> Nothing
  , linked: false
  }
  where
  radius = 350.0
  -- x runs once round the circle (0 to 1), y from the centre to the rim.
  placed = Placed.partition (Partition.defaultPartitionConfig { size = { width: 1.0, height: 1.0 } }) (counted flare)

  rest :: Placement -> Point
  rest node =
    if node.depth == 0 then middle
    else polar (2.0 * pi * node.x) (radius * node.y)
