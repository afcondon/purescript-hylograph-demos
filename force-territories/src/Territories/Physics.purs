-- | The physics: one graph, two sets of forces.
-- |
-- | In open country the characters are an ordinary force-directed graph:
-- | links pull, charges push, discs collide. In territories the same three
-- | forces act, and three more enforce the map: each character is confined
-- | to its territory, drawn toward the others in it, and each crossing is
-- | held at its place between its territories.
module Territories.Physics
  ( Mode(..)
  , allModes
  , label
  , Body
  , BodyFields
  , bodies
  , physics
  , cooling
  , links
  , atRest
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (null)
import Data.Number (cos, pi, sin, sqrt)
import Hylograph.ForceEngine.Setup (LinkEnds, Region(..), Setup, cohesion, collide, containEach, dynamic, link, manyBody, positionX, positionY, setupWithParams, static, towards, withDegreeStrength, withDistance, withLinkDistance, withRadius, withStrength, withTheta, withX, withY)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Territories.Geography (Geography, Point, stage)
import Territories.Story (Character, Story)

data Mode
  = Territories
  | OpenCountry

derive instance Eq Mode

allModes :: Array Mode
allModes = [ Territories, OpenCountry ]

label :: Mode -> String
label = case _ of
  Territories -> "Territories"
  OpenCountry -> "Open country"

-- | A character as the simulation carries it.
type Body = SimulationNode BodyFields

type BodyFields =
  ( character :: Character
  , radius :: Number
  , home :: Maybe Int -- its territory, unless it is a crossing
  , post :: Maybe Int -- its crossing, if it is one
  , remembered :: Maybe { territories :: Point, openCountry :: Point } -- see `physics`
  )

-- | The characters, each starting inside its territory (on a sunflower
-- | spiral round the middle, in the order of the story) or on its crossing.
bodies :: Geography -> Story -> Array Body
bodies geography story = story.characters # Array.mapWithIndex \index character ->
  let
    home = Map.lookup character geography.home
    crossing = Array.find (\candidate -> candidate.character == character) geography.crossings
    territory = home >>= Array.index geography.territories
    start = case crossing, territory of
      Just place, _ -> { x: place.x, y: place.y }
      Nothing, Just land ->
        let
          neighbours = Array.filter (\member -> Map.lookup member geography.home == home) land.members
          place = toNumber (fromMaybe 0 (Array.elemIndex character neighbours))
          distance = 0.7 * land.radius * sqrt ((place + 0.5) / toNumber (max 1 (Array.length neighbours)))
          angle = place * pi * (3.0 - sqrt 5.0)
        in
          { x: land.x + distance * cos angle, y: land.y + distance * sin angle }
      Nothing, Nothing -> { x: stage.width / 2.0, y: stage.height / 2.0 }
  in
    { id: index
    , x: start.x
    , y: start.y
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , character
    , radius: if isJust crossing then 7.0 else 4.5
    , home
    , post: Array.findIndex (\candidate -> candidate.character == character) geography.crossings
    , remembered: Nothing
    }

-- | The story's meetings as the simulation's links, by node id.
links :: Story -> Array { source :: Int, target :: Int }
links story = story.meetings # Array.mapMaybe \meeting -> do
  source <- Array.elemIndex meeting.source story.characters
  target <- Array.elemIndex meeting.target story.characters
  pure { source, target }

-- | The forces of a mode. Alpha is held steady, so a resting place is a
-- | balance of the forces and not the point at which they were switched off.
-- |
-- | `remembered`. A force-directed graph has many resting places, and which
-- | one it finds depends on where it starts; and a linked graph held at a
-- | steady alpha has, in open country, no resting place at all: it turns,
-- | slowly, for ever (the link force is d3's, which is not free of torque).
-- | So a character can carry the place it came to in each mode the first
-- | time (found by `Main` before anything is drawn, with `remembered` empty,
-- | the open country by cooling in the usual way), and a weak spring draws
-- | it there. The spring is weak (`rememberedStrength`); it decides which
-- | resting place, and stops the turning.
physics :: Geography -> Mode -> Setup Body
physics geography mode = setupWithParams (label mode) (shared <> particular)
  { alpha: steadyAlpha, alphaTarget: steadyAlpha, alphaDecay: 0.0, alphaMin: 0.001, velocityRetention: 0.6 }
  where
  -- The ordinary force-directed graph. The charge is summed pair by pair
  -- (theta 0): with 77 characters the approximation would save nothing, and
  -- its cells would make the forces jump as characters cross them.
  shared =
    [ manyBody "charge" # withStrength (static (-150.0)) # withTheta 0.0
    , collide "discs" # withRadius (dynamic \body -> body.radius + 2.0) # withStrength (static 0.7)
    , towards "remembered" (\body -> body.remembered <#> remembering) # withStrength (static rememberedStrength)
    ]
  remembering = case mode of
    Territories -> _.territories
    OpenCountry -> _.openCountry

  particular = case mode of
    OpenCountry ->
      [ link "meetings" # withDistance (static meetingLength) # withDegreeStrength
      , positionX "middle-x" # withX (static (stage.width / 2.0)) # withStrength (static 0.04)
      , positionY "middle-y" # withY (static (stage.height / 2.0)) # withStrength (static 0.06)
      ]
    Territories ->
      [ link "meetings" # withLinkDistance (dynamic reach) # withDegreeStrength
      -- A character is kept in its territory, and a crossing on its post:
      -- a small circle where the layout put it, between its territories.
      , containEach "borders" (lands <> posts) (\body -> body.home <|> (body.post <#> (_ + Array.length lands)))
          # withRadius (dynamic \body -> if isJust body.post then 0.0 else body.radius + 2.0)
          # withStrength (static 0.5)
      , cohesion "kin" _.home # withStrength (static 0.03)
      ]
  lands = geography.territories <#> \land -> CircleRegion { x: land.x, y: land.y, radius: land.radius }
  posts = geography.crossings <#> \crossing -> CircleRegion { x: crossing.x, y: crossing.y, radius: postRadius }

  -- In territories a meeting with a crossing has to reach across the
  -- crossing's plaza and into the territory: its natural length is from the
  -- crossing to two thirds of the way to the territory's middle. Between two
  -- crossings it is the distance between them.
  reach :: LinkEnds Body -> Number
  reach ends = case crossingOf ends.source, crossingOf ends.target of
    Nothing, Nothing -> meetingLength
    Just one, Just other -> sqrt ((one.x - other.x) * (one.x - other.x) + (one.y - other.y) * (one.y - other.y))
    Just crossing, Nothing -> across crossing ends.target
    Nothing, Just crossing -> across crossing ends.source
  crossingOf body = body.post >>= Array.index geography.crossings
  across crossing body = case body.home >>= Array.index geography.territories of
    Just land -> crossing.plaza + land.radius * 2.0 / 3.0
    Nothing -> meetingLength

-- | The alpha every force is scaled by, throughout. At 1 the links are stiff
-- | enough to shake the graph; at a half it walks.
steadyAlpha :: Number
steadyAlpha = 0.5

-- | The same forces cooled from alpha 1 to nothing in about 300 ticks, as a
-- | force-directed graph is usually laid out. Used once, to find the open
-- | country's resting place.
cooling :: Setup Body -> Setup Body
cooling setup = setup { params = setup.params { alpha = 1.0, alphaTarget = 0.0, alphaDecay = 0.0228 } }

-- | The strength of the spring to a remembered place. At 0.04 the walk there
-- | and back does not repeat (the characters find other resting places); from
-- | 0.08 it does, after one round trip.
rememberedStrength :: Number
rememberedStrength = 0.1

-- | The natural length of a meeting between two characters.
meetingLength :: Number
meetingLength = 46.0

-- | How far a crossing may stray from the point the layout gave it.
postRadius :: Number
postRadius = 3.0

-- | At rest: no character moved further than a fiftieth of a pixel in the
-- | last tick, and none is still being accelerated.
atRest :: { step :: Number, acceleration :: Number }
atRest = { step: 0.02, acceleration: 0.005 }
