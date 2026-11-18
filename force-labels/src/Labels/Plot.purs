-- | The plot: where each nation's dot goes in a year, and the rectangle its
-- | label needs. Pure geometry; nothing here knows about the simulation.
module Labels.Plot
  ( Dot
  , Box
  , Point
  , stage
  , frame
  , dotsIn
  , incomeTicks
  , lifeTicks
  , across
  , up
  , leader
  , overlapping
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (log, sqrt)
import Data.String.CodePoints as String
import Labels.Nations (Continent, Era, Nation, readingIn)

type Point = { x :: Number, y :: Number }

-- | A rectangle by its middle and half its width and height.
type Box = { x :: Number, y :: Number, halfWidth :: Number, halfHeight :: Number }

-- | A nation in a year: its dot, and the box its label needs.
type Dot =
  { index :: Int -- the nation's place in the data, the same in every year
  , name :: String
  , continent :: Continent
  , x :: Number
  , y :: Number
  , radius :: Number
  , label :: { halfWidth :: Number, halfHeight :: Number }
  , home :: Point -- where the label would be with nothing in its way: just right of the dot
  }

-- | The drawing surface, and the rectangle the data is plotted in.
stage :: { width :: Number, height :: Number }
stage = { width: 1200.0, height: 760.0 }

frame :: { left :: Number, right :: Number, top :: Number, bottom :: Number }
frame = { left: 64.0, right: 1180.0, top: 18.0, bottom: 712.0 }

-- | Income per person, on a logarithmic scale from $80 to $200,000, and life
-- | expectancy from nothing to 95 years. Both run further than the data
-- | does: the labels of a crowd of dots need somewhere to go, and a crowd
-- | against the edge of the plot has nowhere.
across :: Number -> Number
across income = frame.left + (frame.right - frame.left) * (log income - log 80.0) / (log 200000.0 - log 80.0)

up :: Number -> Number
up years = frame.bottom - (frame.bottom - frame.top) * years / 95.0

incomeTicks :: Array Number
incomeTicks = [ 100.0, 200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0, 100000.0 ]

lifeTicks :: Array Number
lifeTicks = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0 ]

-- | The label's type: Menlo at 9.5 px. Menlo is monospaced, every character
-- | 1233/2048 of the type size wide, so a label's width is COMPUTED from its
-- | length, not measured in the page. (`check.mjs` measures the drawn text
-- | and holds it to this.)
typeSize :: Number
typeSize = 9.5

characterWidth :: Number
characterWidth = typeSize * 1233.0 / 2048.0

-- | The `howMany` most populous of the nations that have a reading for the
-- | year.
dotsIn :: Int -> Era -> Array Nation -> Array Dot
dotsIn howMany era nations = Array.sortWith _.index $ Array.take howMany $ Array.sortWith (negate <<< _.radius) $ Array.catMaybes $ nations # Array.mapWithIndex \index nation ->
  readingIn era nation <#> \reading ->
    let
      x = across reading.income
      y = up reading.lifeExpectancy
      radius = 1.5 + sqrt reading.population / 6000.0
      label =
        { halfWidth: characterWidth * toNumber (String.length nation.name) / 2.0 + 1.5
        , halfHeight: typeSize / 2.0 + 1.5
        }
    in
      { index
      , name: nation.name
      , continent: nation.continent
      , x
      , y
      , radius
      , label
      , home: { x: x + radius + 3.0 + label.halfWidth, y }
      }

-- | The line from a dot to the nearest point of its label's box, if the box
-- | is further from the dot's edge than `slack`.
leader :: Number -> { x :: Number, y :: Number, radius :: Number } -> Box -> Maybe { from :: Point, to :: Point, length :: Number }
leader slack dot box =
  let
    nearest = { x: clamp (box.x - box.halfWidth) (box.x + box.halfWidth) dot.x, y: clamp (box.y - box.halfHeight) (box.y + box.halfHeight) dot.y }
    distance = sqrt ((nearest.x - dot.x) * (nearest.x - dot.x) + (nearest.y - dot.y) * (nearest.y - dot.y))
  in
    if distance - dot.radius > slack then Just { from: { x: dot.x, y: dot.y }, to: nearest, length: distance - dot.radius }
    else Nothing

-- | Do two boxes overlap?
overlapping :: Box -> Box -> Boolean
overlapping one other =
  (one.halfWidth + other.halfWidth) - abs (one.x - other.x) > 0.0
    && (one.halfHeight + other.halfHeight) - abs (one.y - other.y) > 0.0
  where
  abs n = if n < 0.0 then negate n else n
