-- | The physics: every dot and every label is a rectangle in the simulation.
-- |
-- | A dot is pinned where the data puts it. A label is free: a spring draws
-- | it to its home just right of its dot, the rectangle collision keeps it
-- | off every other label and every dot, and containment keeps it in the
-- | plot. The spring cools away to nothing, and what is left at rest is the
-- | collision alone, which leaves nothing overlapping. Pinning the labels at their homes instead shows the overlap the
-- | collision prevents.
module Labels.Physics
  ( Body
  , BodyFields
  , Part(..)
  , Placing(..)
  , allPlacings
  , placingLabel
  , bodies
  , physics
  , Stage(..)
  , firstStage
  , nextStage
  , forcesOf
  , atRest
  , boxOf
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Tuple (Tuple(..))
import Hylograph.ForceEngine.Setup (Region(..), Setup, contain, forceName, dynamic, manyBody, rectCollide, setupWithParams, static, towards, withDistanceMax, withFilter, withHalfHeight, withHalfWidth, withIterations, withRegion, withStrength, withTheta, withThroughSqueeze)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Labels.Nations (Continent)
import Labels.Plot (Box, Dot, Point, frame)

-- | Where the labels are put.
data Placing
  = Free -- by the forces
  | AtTheirDots -- at home, whatever is in the way

derive instance Eq Placing

allPlacings :: Array Placing
allPlacings = [ Free, AtTheirDots ]

placingLabel :: Placing -> String
placingLabel = case _ of
  Free -> "Labels free"
  AtTheirDots -> "Labels fixed at their points"

-- | What a body is of a nation.
data Part
  = ItsDot
  | ItsLabel

derive instance Eq Part

type Body = SimulationNode BodyFields

type BodyFields =
  ( part :: Part
  , nation :: Int -- the nation's place in the data
  , name :: String
  , continent :: Continent
  , radius :: Number -- of the dot (the label's body carries its dot's)
  , halfWidth :: Number -- of the rectangle as drawn
  , halfHeight :: Number
  , home :: Point -- a label's home; a dot's own place
  , dot :: Point -- where the nation's dot is
  )

-- | The rectangle a body is drawn as.
boxOf :: Body -> Box
boxOf body = { x: body.x, y: body.y, halfWidth: body.halfWidth, halfHeight: body.halfHeight }

-- | The dots of a year and their labels, as bodies. A label that is already
-- | in the picture (in `before`) starts where it is, moving as it was; a new
-- | one starts at its home.
bodies :: Array Body -> Array Dot -> Array Body
bodies before dots = (dots <#> asDot) <> (dots <#> asLabel)
  where
  earlier = Map.fromFoldable (Array.filter (\body -> body.part == ItsLabel) before <#> \body -> Tuple body.nation body)
  asDot dot =
    { id: dot.index
    , x: dot.x
    , y: dot.y
    , vx: 0.0
    , vy: 0.0
    , fx: notNull dot.x
    , fy: notNull dot.y
    , part: ItsDot
    , nation: dot.index
    , name: dot.name
    , continent: dot.continent
    , radius: dot.radius
    , halfWidth: dot.radius
    , halfHeight: dot.radius
    , home: { x: dot.x, y: dot.y }
    , dot: { x: dot.x, y: dot.y }
    }
  asLabel dot =
    let
      start = case Map.lookup dot.index earlier of
        Just body -> { x: body.x, y: body.y, vx: body.vx, vy: body.vy }
        Nothing -> { x: dot.home.x, y: dot.home.y, vx: 0.0, vy: 0.0 }
    in
      { id: labelIds + dot.index
      , x: start.x
      , y: start.y
      , vx: start.vx
      , vy: start.vy
      , fx: null
      , fy: null
      , part: ItsLabel
      , nation: dot.index
      , name: dot.name
      , continent: dot.continent
      , radius: dot.radius
      , halfWidth: dot.label.halfWidth
      , halfHeight: dot.label.halfHeight
      , home: dot.home
      , dot: { x: dot.x, y: dot.y }
      }

-- | A label's id is its nation's place in the data plus this.
labelIds :: Int
labelIds = 1000

-- | The forces for a way of placing the labels.
-- |
-- | Alpha cools from 1 to nothing. The spring and the charges are scaled by
-- | alpha and the collision is not, so the labels are arranged while it is
-- | warm and, as it cools, the collision is left alone with them: nothing
-- | presses two rectangles together any more, and whatever still overlaps
-- | comes apart.
physics :: Placing -> Setup Body
physics = arranged true

-- | The forces of `Free` without the collision: what carries the labels out
-- | of the crowds of dots before the collision is allowed to hold them.
spreading :: Setup Body
spreading = arranged false Free

arranged :: Boolean -> Placing -> Setup Body
arranged colliding placing = setupWithParams (placingLabel placing) (Array.filter wanted forces)
  { alpha: 1.0, alphaTarget: 0.0, alphaDecay: cooling, alphaMin: 0.001, velocityRetention: 0.4 }
  where
  wanted force = colliding || forceName force /= boxes
  isLabel body = body.part == ItsLabel
  -- Forces are applied in the order of their names, hence the numbers: the
  -- collision sees what the spring and the charges have asked for.
  home = towards "1 home" (\body -> if isLabel body then Just body.home else Nothing)
  forces = case placing of
    AtTheirDots -> [ home # withStrength (static 0.3) ]
    Free ->
      [ home # withStrength (static homeStrength)
      -- Two charges arrange the labels before the collision holds them. A
      -- dot repels every label from a short way off, which carries the
      -- labels out of a crowd of dots to where there is room; and the labels
      -- repel each other, along the line between their middles. The
      -- collision would resolve without them (a squeezed rectangle goes
      -- through what squeezes it), but it resolves a crowd by walking labels
      -- out of it, further than they need to go: with the charges the
      -- leaders are shorter by a fifth. (In each the filter says who exerts
      -- the charge: a body it leaves out exerts none and is still pushed.
      -- The charges are summed pair by pair, theta 0: the approximation's
      -- cells make the forces jump as bodies cross them.)
      , manyBody "2 clear of the dots"
          # withStrength (static dotCharge)
          # withFilter (\body -> body.part == ItsDot)
          # withDistanceMax dotReach
          # withTheta 0.0
      , manyBody "3 apart"
          # withStrength (static labelCharge)
          # withFilter isLabel
          # withDistanceMax labelReach
          # withTheta 0.0
      -- Every body is a rectangle, and each collides a little larger than
      -- it is drawn, so that what is drawn has air round it. A dot is pinned,
      -- so it is never moved and a label that overlaps it takes the whole
      -- push; two labels share theirs, the larger moving less. A label
      -- pushed from both sides (between two dots, or in a column of labels
      -- with a dot at each end) goes through one of them: without that the
      -- pushes cancel and it sits on both for ever.
      , rectCollide boxes
          # withHalfWidth (dynamic \body -> body.halfWidth + margin)
          # withHalfHeight (dynamic \body -> body.halfHeight + margin)
          # withStrength (static 1.0)
          # withIterations 4
          # withThroughSqueeze true
      -- A label stays in the plot, by its own width and height.
      , contain "5 plot" (RectRegion { x0: frame.left, y0: frame.top, x1: frame.right, y1: frame.bottom })
          # withRegion (\body -> if isLabel body then Just 0 else Nothing)
          # withHalfWidth (dynamic _.halfWidth)
          # withHalfHeight (dynamic _.halfHeight)
          # withStrength (static 1.0)
      ]

boxes :: String
boxes = "4 boxes"

-- | How much larger than it is drawn a rectangle collides.
margin :: Number
margin = 1.25

-- | The spring to a label's home, the charge of a dot and of a label, how
-- | far each charge reaches, and how fast alpha cools. Found by trial on the
-- | four years: weaker charges leave more for the collision to walk apart,
-- | stronger ones carry the labels further than they need to go.
homeStrength :: Number
homeStrength = 0.06

dotCharge :: Number
dotCharge = -60.0

dotReach :: Number
dotReach = 90.0

labelCharge :: Number
labelCharge = -90.0

labelReach :: Number
labelReach = 60.0

cooling :: Number
cooling = 0.02

-- | What the labels are doing after a change. Free labels are arranged in
-- | three stages, each a setup applied to the running simulation:
-- | they go home (so that the arrangement does not depend on where they
-- | were), they are spread by the charges, and then they are packed: the
-- | collisions are turned on.
data Stage
  = Gathering Int -- ticks left
  | Spreading Int
  | Packing

derive instance Eq Stage

-- | The stages a change begins with, and the forces of each.
firstStage :: Placing -> Stage
firstStage = case _ of
  Free -> Gathering stageTicks
  AtTheirDots -> Packing

-- | The stage after a tick of this one, and whether the forces change.
nextStage :: Stage -> { stage :: Stage, changed :: Boolean }
nextStage = case _ of
  Gathering left -> if left > 1 then { stage: Gathering (left - 1), changed: false } else { stage: Spreading stageTicks, changed: true }
  Spreading left -> if left > 1 then { stage: Spreading (left - 1), changed: false } else { stage: Packing, changed: true }
  Packing -> { stage: Packing, changed: false }

forcesOf :: Placing -> Stage -> Setup Body
forcesOf placing = case _ of
  Gathering _ -> physics AtTheirDots
  Spreading _ -> spreading
  Packing -> physics placing

stageTicks :: Int
stageTicks = 40

-- | Still enough to stop: no label moved further than a twentieth of a
-- | pixel in the last tick.
atRest :: { step :: Number, acceleration :: Number }
atRest = { step: 0.05, acceleration: 0.1 }
