-- | The physics: a force-directed graph in which every import points down.
-- |
-- | The usual three forces (links, charge, collision) and one more: the
-- | directional constraint, which pushes each link's target to be a gap
-- | below its source. Nothing gives a module a row to stand in; a module
-- | ends up below everything that imports it because every one of those
-- | links pushes it there.
module Downhill.Physics
  ( Body
  , BodyFields
  , Point
  , bodies
  , physics
  , stage
  , wantedDrop
  , askedDrop
  , downhill
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Number (cos, pi, sin, sqrt)
import Data.String (Pattern(..))
import Data.String as String
import Downhill.Graph (Graph)
import Hylograph.ForceEngine.Setup (Setup, directional, dynamic, link, manyBody, positionX, positionY, rectCollide, setupWithParams, static, withDegreeStrength, withDistance, withGap, withHalfHeight, withHalfWidth, withIterations, withStrength, withTheta, withX, withY)
import Hylograph.ForceEngine.Simulation (SimulationNode)

type Point = { x :: Number, y :: Number }

-- | A module as the simulation carries it: a box with its name in.
type Body = SimulationNode BodyFields

type BodyFields =
  ( name :: String
  , label :: String -- the last two words of its name, which is what is drawn
  , family :: Int
  , halfWidth :: Number
  , halfHeight :: Number
  )

stage :: { width :: Number, height :: Number }
stage = { width: 1200.0, height: 760.0 }

-- | The modules, starting on a sunflower spiral round the middle of the
-- | stage in the order of their names: no module is told which way is down.
bodies :: Graph -> Array Body
bodies graph = graph.modules # Array.mapWithIndex \index mod ->
  let
    place = toNumber index
    distance = 14.0 * sqrt (place + 0.5)
    angle = place * pi * (3.0 - sqrt 5.0)
    label = lastWords mod.short
  in
    { id: index
    , x: stage.width / 2.0 + distance * cos angle
    , y: stage.height / 2.0 + distance * sin angle
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , name: mod.name
    , label
    -- Menlo at 9 px is 1233/2048 of 9 px a character: computed, not measured.
    , halfWidth: 9.0 * 1233.0 / 2048.0 * toNumber (String.length label) / 2.0 + 5.0
    , halfHeight: 8.0
    , family: mod.family
    }

-- | The last two words of a module's name: one is not enough to tell the
-- | library's many `Types` and `FFI` modules apart.
lastWords :: String -> String
lastWords short = String.joinWith "." (Array.takeEnd 2 (String.split (Pattern ".") short))

-- | How far below what imports it a module should be: what the picture
-- | promises, and what the force is asked for. The constraint is a spring
-- | like the others (it pushes by the shortfall, scaled by alpha), and at
-- | rest it has given way to what pulls the rows together (the force that
-- | keeps the graph on the stage, most of all); it is asked for more so that
-- | what is promised is kept.
wantedDrop :: Number
wantedDrop = 40.0

askedDrop :: Number
askedDrop = 78.0

-- | Alpha cools from 1 to nothing in about 300 ticks, as a force-directed
-- | graph is usually laid out: a run has a beginning and an end, which is
-- | what the timeline shows.
physics :: Setup Body
physics = setupWithParams "downhill"
  [ link "imports" # withDistance (static 70.0) # withDegreeStrength
  , manyBody "charge" # withStrength (static (-240.0)) # withTheta 0.0
  , rectCollide "boxes"
      # withHalfWidth (dynamic \body -> body.halfWidth + 5.0)
      # withHalfHeight (dynamic \body -> body.halfHeight + 5.0)
      # withIterations 2
  , directional "downhill" # withGap (static askedDrop) # withStrength (static 1.0) # withIterations 5
  , positionX "middle" # withX (static (stage.width / 2.0)) # withStrength (static 0.055)
  , positionY "level" # withY (static (stage.height / 2.0)) # withStrength (static 0.09)
  ]
  { alpha: 1.0, alphaTarget: 0.0, alphaDecay: 0.0228, alphaMin: 0.001, velocityRetention: 0.6 }

-- | How many of the imports have their target at least `wantedDrop` below
-- | their source.
downhill :: Graph -> Array Point -> Int
downhill graph places = Array.length $ graph.imports # Array.filter \needs ->
  case Array.index places needs.source, Array.index places needs.target of
    Just source, Just target -> target.y - source.y >= wantedDrop
    _, _ -> false
