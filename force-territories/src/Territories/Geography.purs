-- | The geography: the graph's decomposition into biconnected blocks, laid
-- | out as land. Pure; nothing here knows about the simulation.
-- |
-- | A block is a set of characters no single one of whom can be removed to
-- | disconnect the rest: it becomes a territory, a circle sized by its
-- | members. An articulation point is a character who belongs to several
-- | blocks and is the only way between them: it becomes a crossing, a point
-- | with all of its territories round it. A bridge is a block of two.
-- |
-- | The layout is of the block-cut tree, which is a tree whatever the graph:
-- | the largest territory in the middle, each crossing just outside the rim
-- | of the territory nearer the middle, and the crossing's other territories
-- | fanned round it on the far side. Circles round a common point overlap
-- | unless they stand back from it, so each crossing has a plaza: the
-- | smallest circle round it that all of its territories can touch without
-- | touching each other.
-- |
-- | (Why not `DataViz.Layout.BlockCutTree` or a circle packing? The first
-- | puts blocks in rows by distance from the root and the second packs them
-- | by size; in neither do two territories that share a character meet at
-- | that character, which is the one thing this picture has to show.)
module Territories.Geography
  ( Geography
  , Territory
  , Crossing
  , Point
  , geography
  , stage
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximum, maximumBy, minimum, sum)
import Data.Graph.Algorithms (mkSimpleGraph)
import Data.Graph.Decomposition (blockCutTree)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (asin, cos, pi, sin, sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Territories.Story (Character, Meeting, Story)

type Point = { x :: Number, y :: Number }

type Territory =
  { index :: Int
  , x :: Number
  , y :: Number
  , radius :: Number
  , members :: Array Character -- crossings included
  , bridge :: Boolean -- a block of two: one meeting, and no other way round
  }

type Crossing =
  { character :: Character
  , x :: Number
  , y :: Number
  , plaza :: Number -- the radius of the circle its territories stand round
  , territories :: Array Int
  }

type Geography =
  { territories :: Array Territory
  , crossings :: Array Crossing
  , home :: Map Character Int -- the one territory of a character who is not a crossing
  , bridges :: Array Meeting
  }

-- | The drawing surface.
stage :: { width :: Number, height :: Number }
stage = { width: 1200.0, height: 760.0 }

-- | A territory's radius from the number of its members: 12 for a bridge,
-- | and area in proportion to the members beyond two. (In the map's own
-- | units; `geography` scales the finished map to the stage.)
radiusFor :: Int -> Number
radiusFor members = 12.0 + 19.0 * sqrt (toNumber (max 0 (members - 2)))

-- | The space left round the map on the stage.
margin :: Number
margin = 36.0

-- | The smallest plaza a crossing may have: room for its own disc.
smallestPlaza :: Number
smallestPlaza = 9.0

-- | The share of the full turn round a crossing that its territories may
-- | fill; the rest is the gaps between them.
fullness :: Number
fullness = 0.9

-- | Seen from a point at `distance` from its rim, a circle fills this angle
-- | to either side of the line to its centre.
halfAngle :: Number -> Number -> Number
halfAngle distance radius = asin (radius / (distance + radius))

-- | The smallest plaza round which circles of these radii fit side by side.
plazaFor :: Array Number -> Number
plazaFor radii = go 40 smallestPlaza 2000.0
  where
  fits plaza = sum (radii <#> \radius -> 2.0 * halfAngle plaza radius) <= 2.0 * pi * fullness
  go :: Int -> Number -> Number -> Number
  go steps low high
    | fits low = low
    | steps == 0 = high
    | otherwise =
        let
          middle = (low + high) / 2.0
        in
          if fits middle then go' (steps - 1) low middle else go (steps - 1) middle high
  -- `low` does not fit and `high` does: close in on the boundary.
  go' steps low high
    | steps == 0 = high
    | otherwise =
        let
          middle = (low + high) / 2.0
        in
          if fits middle then go' (steps - 1) low middle else go' (steps - 1) middle high

geography :: Story -> Geography
geography story =
  { territories: blocks # Array.mapWithIndex \index members ->
      let
        centre = fromMaybe origin (Map.lookup index placed.territories)
      in
        { index
        , x: fit.x centre.x
        , y: fit.y centre.y
        , radius: fit.scale * radiusOf index
        , members: Set.toUnfoldable members
        , bridge: Set.size members == 2
        }
  , crossings: Set.toUnfoldable decomposition.cutVertices <#> \character ->
      let
        point = fromMaybe origin (Map.lookup character placed.crossings)
      in
        { character
        , x: fit.x point.x
        , y: fit.y point.y
        , plaza: fit.scale * plazaOf character
        , territories: territoriesOf character
        }
  , home: Map.fromFoldable $ story.characters # Array.mapMaybe \character ->
      if Set.member character decomposition.cutVertices then Nothing
      else Tuple character <$> Array.head (territoriesOf character)
  , bridges: story.meetings # Array.filter \meeting ->
      Array.any (\members -> Set.size members == 2 && Set.member meeting.source members && Set.member meeting.target members) blocks
  }
  where
  origin = { x: 0.0, y: 0.0 }
  decomposition = blockCutTree $ mkSimpleGraph story.characters
    (story.meetings <#> \meeting -> Tuple meeting.source meeting.target)
  blocks = decomposition.blocks

  radiusOf :: Int -> Number
  radiusOf index = radiusFor (fromMaybe 0 (Set.size <$> Array.index blocks index))

  territoriesOf :: Character -> Array Int
  territoriesOf character = Array.mapMaybe identity $ blocks # Array.mapWithIndex \index members ->
    if Set.member character members then Just index else Nothing

  crossingsIn :: Int -> Array Character
  crossingsIn index = case Array.index blocks index of
    Just members -> Set.toUnfoldable (Set.intersection members decomposition.cutVertices)
    Nothing -> []

  plazaOf :: Character -> Number
  plazaOf character = plazaFor (radiusOf <$> territoriesOf character)

  -- The largest territory is the middle of the map.
  capital = fromMaybe 0 $ _.index <$> maximumBy (comparing _.size)
    (blocks # Array.mapWithIndex \index members -> { index, size: Set.size members })

  placed = settle capital origin Nothing { territories: Map.empty, crossings: Map.empty }

  -- How much land lies beyond a crossing, away from the territory `from`:
  -- what decides the share of a rim the crossing is given.
  beyond :: Int -> Character -> Number
  beyond from character =
    plazaOf character + sum (Array.filter (_ /= from) (territoriesOf character) <#> \index ->
      2.0 * radiusOf index + sum (Array.filter (_ /= character) (crossingsIn index) <#> beyond index))

  -- Put a territory at a point, then everything that hangs off it. `via` is
  -- the crossing it was reached through and the direction that crossing lies
  -- in, seen from the territory's centre; the capital has none.
  settle
    :: Int
    -> Point
    -> Maybe { crossing :: Character, direction :: Number }
    -> { territories :: Map Int Point, crossings :: Map Character Point }
    -> { territories :: Map Int Point, crossings :: Map Character Point }
  settle index centre via soFar =
    foldl hang (soFar { territories = Map.insert index centre soFar.territories }) (Array.zip onward directions)
    where
    onward = Array.filter (\character -> Just character /= (via <#> _.crossing)) (crossingsIn index)
    weights = beyond index <$> onward
    total = sum weights

    -- Each crossing gets a share of the rim in proportion to the land beyond
    -- it: of the whole rim for the capital, of the two thirds facing away
    -- from where we came for any other territory.
    arc = case via of
      Nothing -> { from: -pi / 2.0, width: 2.0 * pi }
      Just arrival -> { from: arrival.direction + pi - 2.0 * pi / 3.0, width: 4.0 * pi / 3.0 }
    directions = (Array.zip weights (runningTotals weights)) <#> \(Tuple weight before) ->
      arc.from + arc.width * (before + weight / 2.0) / (if total > 0.0 then total else 1.0)

    hang acc (Tuple character direction) =
      let
        plaza = plazaOf character
        point = polar centre direction (radiusOf index + plaza)
        others = middleOut (Array.sortWith (negate <<< radiusOf) (Array.filter (_ /= index) (territoriesOf character)))
        -- Round the crossing, starting from the edge of the angle this
        -- territory fills behind it, with equal gaps.
        behind = halfAngle plaza (radiusOf index)
        widths = others <#> \other -> 2.0 * halfAngle plaza (radiusOf other)
        gap = (2.0 * pi - 2.0 * behind - sum widths) / toNumber (Array.length others + 1)
        start = direction + pi + behind
        facings = Array.zip widths (runningTotals widths) # Array.mapWithIndex \position (Tuple width before) ->
          start + gap * toNumber (position + 1) + before + width / 2.0
      in
        foldl
          ( \inner (Tuple other facing) ->
              settle other (polar point facing (plaza + radiusOf other)) (Just { crossing: character, direction: facing + pi }) inner
          )
          (acc { crossings = Map.insert character point acc.crossings })
          (Array.zip others facings)

  -- The map is laid out round (0, 0) in units of its own. Fit it to the
  -- stage: as large as leaves a margin, and in the middle.
  fit =
    let
      edges = Array.concatMap
        ( \(Tuple index centre) ->
            [ { x: centre.x - radiusOf index, y: centre.y - radiusOf index }
            , { x: centre.x + radiusOf index, y: centre.y + radiusOf index }
            ]
        )
        (Map.toUnfoldable placed.territories)
      least coordinate = fromMaybe 0.0 (minimum (coordinate <$> edges))
      most coordinate = fromMaybe 1.0 (maximum (coordinate <$> edges))
      scale = min ((stage.width - 2.0 * margin) / (most _.x - least _.x)) ((stage.height - 2.0 * margin) / (most _.y - least _.y))
    in
      { scale
      , x: \x -> stage.width / 2.0 + scale * (x - (least _.x + most _.x) / 2.0)
      , y: \y -> stage.height / 2.0 + scale * (y - (least _.y + most _.y) / 2.0)
      }

polar :: Point -> Number -> Number -> Point
polar centre direction distance =
  { x: centre.x + distance * cos direction, y: centre.y + distance * sin direction }

-- | For each number, the sum of those before it.
runningTotals :: Array Number -> Array Number
runningTotals = Array.scanl (+) 0.0 >>> Array.cons 0.0 >>> Array.dropEnd 1

-- | Largest first becomes largest in the middle: alternate ends.
middleOut :: forall a. Array a -> Array a
middleOut = Array.foldl (\acc item -> if Array.length acc `mod` 2 == 0 then Array.snoc acc item else Array.cons item acc) []
