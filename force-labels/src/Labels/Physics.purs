-- | The physics: every dot and every label is a rectangle in the simulation.
-- |
-- | A dot is pinned where the data puts it. A label is free: a spring draws
-- | it to its home just right of its dot, the rectangle collision keeps it
-- | off every other label and every dot, and containment keeps it in the
-- | plot. Pinning the labels at their homes instead shows the overlap the
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
import Hylograph.ForceEngine.Setup (Region(..), Setup, containEach, forceName, dynamic, manyBody, rectCollide, setupWithParams, static, towards, withDistanceMax, withFilter, withHalfHeight, withHalfWidth, withIterations, withRadius, withStrength, withTheta)
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

-- | The forces for a way of placing the labels, given the bodies they will
-- | act on (containment needs to know each label's width: see below).
-- |
-- | Alpha cools from 1 to `restingAlpha`. The spring and the charges are
-- | scaled by alpha and the collisions are not, so the labels travel quickly
-- | and then, at rest, press on each other only lightly: a collision gives
-- | by as much as it is pressed, and what it gives has to stay inside the
-- | margin.
physics :: Placing -> Array Body -> Setup Body
physics = arranged true

-- | The forces of `Free` without the collisions: what carries the labels out
-- | of the crowds of dots before the collisions are allowed to hold them.
spreading :: Array Body -> Setup Body
spreading = arranged false Free

arranged :: Boolean -> Placing -> Array Body -> Setup Body
arranged colliding placing present = setupWithParams (placingLabel placing) (Array.filter wanted forces)
  { alpha: 1.0, alphaTarget: restingAlpha, alphaDecay: 0.01, alphaMin: 0.001, velocityRetention: 0.4 }
  where
  wanted force = colliding || not (Array.elem (forceName force) [ "boxes: label and label", "boxes: label and dot" ])
  home = towards "home" (\body -> if body.part == ItsLabel then Just body.home else Nothing)
  forces = case placing of
    AtTheirDots -> [ home # withStrength (static 0.3) ]
    Free ->
      [ home # withStrength (static homeStrength)
      -- Each rectangle collides a little larger than it is drawn. The
      -- collision is a spring like the others, and at rest it has given a
      -- little: the margin is what it gives, so that what is DRAWN does not
      -- overlap.
      --
      -- Label against label: an overlapping pair shares the correction, the
      -- larger moving less.
      , rectCollide "boxes: label and label"
          # withHalfWidth (dynamic \body -> onlyLabels body (body.halfWidth + margin))
          # withHalfHeight (dynamic \body -> onlyLabels body (body.halfHeight + margin))
          # withStrength (static 1.0)
          # withIterations 4
      -- Label against dot: the label takes (nearly) all of the correction.
      , rectCollide "boxes: label and dot"
          # withHalfWidth (dynamic \body -> againstDots apart.x body (body.halfWidth + margin))
          # withHalfHeight (dynamic \body -> againstDots apart.y body (body.halfHeight + margin))
          # withStrength (static 1.0)
      -- The collisions alone jam. A label between two dots is pushed up by
      -- one and down by the other; the pushes cancel and it stays on top of
      -- both. The same happens in a column of labels with a dot at each end.
      -- A rectangle is pushed along the axis on which it overlaps least, and
      -- for a wide label that is nearly always up or down, so nothing ever
      -- moves it sideways out of the column.
      --
      -- Two charges do that, and are what find the arrangement; the
      -- collisions then only have to hold it. A dot repels every label from
      -- a short way off, which carries the labels out of a crowd of dots to
      -- where there is room; and the labels repel each other, along the line
      -- between their middles, which is seldom straight up. (In each the
      -- filter says who exerts the charge: a body it leaves out exerts none
      -- and is still pushed. The charges are summed pair by pair, theta 0:
      -- the approximation's cells make the forces jump as bodies cross them,
      -- and the labels then never quite stop.)
      , manyBody "clear of the dots"
          # withStrength (static dotCharge)
          # withFilter (\body -> body.part == ItsDot)
          # withDistanceMax dotReach
          # withTheta 0.0
      , manyBody "apart"
          # withStrength (static labelCharge)
          # withFilter (\body -> body.part == ItsLabel)
          # withDistanceMax labelReach
          # withTheta 0.0
      -- WORKAROUND (hylograph-simulation, `ContainConfig`): containment
      -- keeps a node a `radius` inside its region, and a label is not round.
      -- So each label has a region of its own, the plot narrowed by the
      -- difference between its half-width and half-height, and is kept its
      -- half-height inside that.
      , containEach "plot" (labels <#> ownPlot) (\body -> if body.part == ItsLabel then Map.lookup body.nation regionOf else Nothing)
          # withRadius (dynamic _.halfHeight)
          # withStrength (static 1.0)
      ]
  labels = Array.filter (\body -> body.part == ItsLabel) present
  regionOf = Map.fromFoldable (Array.mapWithIndex (\place body -> Tuple body.nation place) labels)
  ownPlot body =
    let
      inset = body.halfWidth - body.halfHeight
    in
      RectRegion { x0: frame.left + inset, y0: frame.top, x1: frame.right - inset, y1: frame.bottom }

-- | WORKAROUND (hylograph-simulation and both kernels, `rectCollide`).
-- |
-- | The force has no filter, shares every correction between the two
-- | rectangles by area (the smaller takes the more), and does not know that
-- | a rectangle is pinned. A dot is small and pinned: a label overlapping a
-- | dot is given a small share of the push it needs, the dot is given the
-- | rest and ignores it, and at rest the label still covers the dot.
-- |
-- | What is wanted is two collisions: label against label, shared; and label
-- | against dot, the label taking all of it. A pair overlaps when the sum of
-- | its half-extents exceeds the distance between its centres on both axes,
-- | and the half-extents are numbers this demo supplies. So:
-- |
-- | - `onlyLabels`: a dot's half-extents are negative, and large. No sum
-- |   with a dot's is positive: only label and label collide.
-- | - `againstDots`: a label's half-extents are its own less `apart`, a
-- |   dot's its own plus `apart`. Label and dot sum to what they should.
-- |   Label and label sum to less than nothing (`apart` is more than any
-- |   label's half-extent). Dot and dot overlap when they are near each
-- |   other, and both are pinned. A label's area (the product of two
-- |   negative numbers) is now small beside a dot's, so the label takes
-- |   nine tenths or more of the correction.
onlyLabels :: Body -> Number -> Number
onlyLabels body extent = case body.part of
  ItsLabel -> extent
  ItsDot -> -4096.0

againstDots :: Number -> Body -> Number -> Number
againstDots offset body extent = case body.part of
  ItsLabel -> extent - offset
  ItsDot -> extent + offset

-- | More than any label's half-width and half-height (with the margin).
apart :: { x :: Number, y :: Number }
apart = { x: 72.0, y: 8.0 }

-- | How much larger than it is drawn a rectangle collides.
margin :: Number
margin = 1.25

-- | The spring to a label's home, the charge of a dot and of a label, and
-- | how far each charge reaches. Found by trial on the four years, with both
-- | kernels: weaker charges leave labels jammed (see `physics`), stronger
-- | ones carry them further from their dots than they need to go.
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

restingAlpha :: Number
restingAlpha = 0.03

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

forcesOf :: Placing -> Stage -> Array Body -> Setup Body
forcesOf placing = case _ of
  Gathering _ -> physics AtTheirDots
  Spreading _ -> spreading
  Packing -> physics placing

stageTicks :: Int
stageTicks = 40

-- | Still enough to stop: no label moved further than a twentieth of a
-- | pixel in the last tick. (The acceleration is allowed to be larger than
-- | in the other demos: rectangles that touch go on trading pushes of a few
-- | hundredths of a pixel for as long as they are ticked.)
atRest :: { step :: Number, acceleration :: Number }
atRest = { step: 0.05, acceleration: 0.1 }
