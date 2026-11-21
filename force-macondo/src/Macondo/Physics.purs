-- | The physics: one graph, four sets of forces.
-- |
-- | First the family tree: only the Buendías, joined parent to child and
-- | couple to couple, with each child held at least a generation's width to
-- | the right of each parent by `directional` with its hard limit (the force
-- | of "links that point down", here pointing across). Nothing places a
-- | generation; the layers come out of the links.
-- |
-- | In open country the people are an ordinary force-directed graph: links
-- | pull, charges push, discs collide. In each of the other modes the same
-- | forces act on a partition of the people, and two more hold each person in
-- | their part's region and draw them toward the others there:
-- |
-- | - generations: a circle per era, in a row, so the hundred years read left
-- |   to right;
-- | - generations by sex: a grid of rectangles, eras across, men above and
-- |   women below, so each disc sits in the cell of two facts at once.
-- |
-- | The region forces are those of `Film.Regions` in
-- | `hylograph-films/taking-d3-out`.
module Macondo.Physics
  ( Mode(..)
  , allModes
  , label
  , Shape(..)
  , Land
  , Partition
  , Geography
  , geography
  , landsOf
  , Body
  , BodyFields
  , bodies
  , Tie(..)
  , Ties
  , ties
  , degreesOf
  , physics
  , sameForces
  , stage
  , corner
  , generationWidth
  , outline
  , paintOutline
  , inside
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.Number (abs, cos, infinity, pi, pow, sin, sqrt)
import Hylograph.ForceEngine.Setup (ForceConfig, LinkEnds, Region(..), Setup, cohesion, collide, containEach, directional, dynamic, link, manyBody, positionX, positionY, setupWithParams, static, withAxis, withDistance, withDistanceMax, withFilter, withGap, withHardLimit, withIterations, withLinkDistance, withLinkStrength, withRadius, withStrength, withTheta, withX, withY)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Macondo.Story (Person, Sex(..), Story)

-- | The steps of the story. The first two are the same forces; the second
-- | only colours each person by their generation.
data Mode
  = Tree
  | OpenCountry
  | InColour
  | Generations
  | GenerationsBySex

derive instance Eq Mode

allModes :: Array Mode
allModes = [ Tree, OpenCountry, InColour, Generations, GenerationsBySex ]

label :: Mode -> String
label = case _ of
  Tree -> "The tree"
  Generations -> "Generations"
  GenerationsBySex -> "Generations × sex"
  OpenCountry -> "Open country"
  InColour -> "In colour"

-- | The drawing surface.
stage :: { width :: Number, height :: Number }
stage = { width: 1200.0, height: 480.0 }

margin :: Number
margin = 40.0

-- | A region's shape, in stage units.
data Shape
  = Disc { x :: Number, y :: Number, radius :: Number }
  | Box { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

-- | One part of a partition: its shape, its caption and where the caption goes,
-- | and the generation whose colour it wears, if any.
type Land = { name :: String, shape :: Shape, caption :: { x :: Number, y :: Number }, hue :: Maybe Int }

-- | A partition: its lands, and which land each person (by index) is in.
type Partition = { lands :: Array Land, member :: Array (Maybe Int) }

-- | The three partitions, worked out once.
type Geography = { generations :: Partition, grid :: Partition }

geography :: Story -> Geography
geography story =
  { generations: row (roman <$> eras) (\person -> Array.elemIndex person.era eras) identity Just
  , grid
  }
  where
  people = story.people
  eras = Array.nub (Array.sort (_.era <$> people))

  -- Circles in a row, each with area in proportion to its members, scaled to
  -- fill the stage.
  row :: Array String -> (Person -> Maybe Int) -> (String -> String) -> (Int -> Maybe Int) -> Partition
  row names memberOf caption hueOf =
    let
      member = memberOf <$> people
      count index = Array.length (Array.filter (_ == Just index) member)
      radii = Array.mapWithIndex (\index _ -> 14.0 + 10.0 * sqrt (toNumber (count index))) names
      gap = 24.0
      across = sum (radii <#> (_ * 2.0)) + gap * toNumber (Array.length radii - 1)
      tallest = 2.0 * fromMaybe 1.0 (maximum radii)
      scale = min ((stage.width - 2.0 * margin) / across) ((stage.height - 2.0 * margin - 30.0) / tallest)
      left = (stage.width - scale * across) / 2.0
      middle = stage.height / 2.0 + 12.0
      place index name =
        let
          r = fromMaybe 0.0 (Array.index radii index)
          before = sum (Array.take index radii <#> \earlier -> 2.0 * earlier + gap)
          x = left + scale * (before + r)
        in
          { name: caption name
          , shape: Disc { x, y: middle, radius: scale * r }
          , caption: { x, y: middle - scale * r - 10.0 }
          , hue: hueOf index
          }
    in
      { lands: Array.mapWithIndex place names, member }

  -- A rectangle per (era, sex): eras across in equal columns, men above.
  grid :: Partition
  grid =
    let
      columns = Array.length eras
      -- Wide gaps: the wash is a rounded blob that does not reach a cell's
      -- corners, so the cell held by the forces is kept to about the paint.
      gap = 30.0
      top = margin + 10.0
      width = (stage.width - 2.0 * margin - gap * toNumber (columns - 1)) / toNumber columns
      height = (stage.height - top - margin - gap) / 2.0
      cell column sexRow =
        let
          x0 = margin + toNumber column * (width + gap)
          y0 = top + toNumber sexRow * (height + gap)
        in
          { name: (if sexRow == 0 then "hombres · " else "mujeres · ") <> roman (fromMaybe 0 (Array.index eras column))
          , shape: Box { x0, y0, x1: x0 + width, y1: y0 + height }
          , caption: { x: x0 + width / 2.0, y: y0 + 16.0 }
          , hue: Just column
          }
      lands = do
        sexRow <- [ 0, 1 ]
        column <- Array.range 0 (columns - 1)
        pure (cell column sexRow)
      memberOf person = do
        column <- Array.elemIndex person.era eras
        pure (column + columns * (if person.sex == Man then 0 else 1))
    in
      { lands, member: memberOf <$> people }

-- | A generation's number as the old books would set it.
roman :: Int -> String
roman = case _ of
  1 -> "I"
  2 -> "II"
  3 -> "III"
  4 -> "IV"
  5 -> "V"
  6 -> "VI"
  7 -> "VII"
  n -> show n

landsOf :: Geography -> Mode -> Array Land
landsOf atlas = case _ of
  Generations -> atlas.generations.lands
  GenerationsBySex -> atlas.grid.lands
  _ -> []

-- | A person as the simulation carries it.
type Body = SimulationNode BodyFields

type BodyFields =
  ( person :: Person
  , radius :: Number
  , generation :: Maybe Int -- their land in each partition
  , cell :: Maybe Int
  )

-- | A disc's area grows with the passages a person is present in.
radiusFor :: Person -> Number
radiusFor person = 2.5 + 0.65 * sqrt (toNumber person.appearances)

-- | Everyone starts at their corner (`corner`), off the stage, a little apart.
bodies :: Geography -> Story -> Array Body
bodies atlas story = Array.mapWithIndex body story.people
  where
  at partition index = join (Array.index partition.member index)
  body id person =
    let
      angle = toNumber id * pi * (3.0 - sqrt 5.0)
      distance = 6.0 * sqrt (toNumber id + 1.0)
      home = corner { person }
    in
      { id
      , x: home.x + distance * cos angle
      , y: home.y + distance * sin angle
      , vx: 0.0
      , vy: 0.0
      , fx: null
      , fy: null
      , person
      , radius: radiusFor person
      , generation: at atlas.generations id
      , cell: at atlas.grid id
      }

-- | What joins two people. The simulation carries all three kinds in one
-- | array of links, meetings first; each mode decides which of them pull.
data Tie
  = Meeting -- present in the same passages
  | Descent -- parent to child, as the family tree draws it
  | Couple -- the parents of a child, or a childless marriage

derive instance Eq Tie

type Ties = { links :: Array { source :: Int, target :: Int }, kinds :: Array Tie }

ties :: Story -> Ties
ties story =
  { links: (meetings <#> _.link) <> (descent <#> _.link) <> (couples <#> _.link)
  , kinds: (meetings $> Meeting) <> (descent $> Descent) <> (couples $> Couple)
  }
  where
  indexOf id = Array.findIndex (\person -> person.id == id) story.people
  between a b = do
    source <- indexOf a
    target <- indexOf b
    pure { link: { source, target } }
  meetings = story.meetings # Array.mapMaybe \meeting -> between meeting.source meeting.target
  descent = story.people # Array.concatMap \child -> child.parents # Array.mapMaybe \parent -> between parent child.id
  -- A couple runs from the one born into the family to the one who married
  -- in, so that the in-law can be held in their spouse's column.
  couples = story.couples # Array.mapMaybe \couple ->
    if bornIn couple.b && not (bornIn couple.a) then between couple.b couple.a else between couple.a couple.b
  bornIn id = maybe false (not <<< Array.null <<< _.parents) (Array.find (\person -> person.id == id) story.people)

-- | Each person's number of meetings, by index.
degreesOf :: Story -> Array Int
degreesOf story = Array.range 0 (Array.length story.people - 1) <#> \index ->
  Array.length (Array.filter (\meeting -> meeting.source == index || meeting.target == index) meetingLinks)
  where
  together = ties story
  meetingLinks = Array.catMaybes (Array.zipWith (\l k -> if k == Meeting then Just l else Nothing) together.links together.kinds)

-- | Whether two modes are held by the same forces, so that going from one to
-- | the other changes only how the people are drawn.
sameForces :: Mode -> Mode -> Boolean
sameForces a b = forces a == forces b
  where
  forces = case _ of
    InColour -> OpenCountry
    other -> other

-- | The forces of a mode, cooled from alpha 1 to nothing, as a force-directed
-- | graph usually is (some 290 ticks, a little faster than d3's 300).
-- | Choosing a mode starts the cooling again.
-- |
-- | Within a partition a meeting inside one land pulls with d3's usual
-- | strength (one over the smaller of the two degrees); a meeting between
-- | lands with a tenth of it. At full strength the meetings between lands pin
-- | everyone to the rims nearest the other lands, and the middles stay empty.
-- | `degrees` is each person's number of meetings, by index.
physics :: Geography -> Array Int -> Array Tie -> Set Int -> Mode -> Setup Body
physics atlas degrees kinds arrived mode = setupWithParams (label mode) (shared <> particular)
  { alpha: 1.0, alphaTarget: 0.0, alphaDecay: 0.024, alphaMin: 0.001, velocityRetention: 0.6 }
  where
  -- Open country needs more room than a land does: at the lands' charge the
  -- whole cast bunches into one knot in the middle.
  charge = case mode of
    Tree -> -160.0
    OpenCountry -> -160.0
    InColour -> -160.0
    _ -> -60.0
  -- Only those who have arrived are on stage (in the tree, only Buendías
  -- arrive); the rest wait, unseen, at their corner (`corner`), push no one,
  -- collide with nothing, and pull and are pulled by no tie.
  onStage :: Body -> Boolean
  onStage body = Set.member body.id arrived && (mode /= Tree || body.person.family)
  -- In the tree the charge only reaches a sibling's distance: at full reach
  -- every Buendía pushes every other, and the chain of generations is pushed
  -- apart lengthwise until the tree no longer fits.
  reach = case mode of
    Tree -> 110.0
    _ -> infinity
  shared =
    [ manyBody "charge" # withStrength (static charge) # withTheta 0.0 # withFilter onStage # withDistanceMax reach
    , collide "discs" # withRadius (dynamic \body -> body.radius + 2.0) # withStrength (static 0.7) # withFilter onStage
    , positionX "waiting-x" # withX (dynamic (_.x <<< waiting)) # withStrength (static 0.3) # withFilter (not <<< onStage)
    , positionY "waiting-y" # withY (dynamic (_.y <<< waiting)) # withStrength (static 0.3) # withFilter (not <<< onStage)
    ]
  particular = case mode of
    Tree ->
      [ link "ties" # withLinkDistance (dynamic treeLength) # withLinkStrength (dynamic treeStrength)
      , directional "descent"
          # withAxis { x: 1.0, y: 0.0 }
          # withGap (dynamic descentGap)
          # withLinkStrength (dynamic \ends -> if tieOf ends == Descent then 1.0 else 0.0)
          # withIterations 5
          # withHardLimit true
      -- An in-law no further left than the spouse they married into: with
      -- the westward pull, that is exactly the spouse's column.
      , directional "in-laws"
          # withAxis { x: 1.0, y: 0.0 }
          # withGap (dynamic \ends -> if tieOf ends == Couple && bothOnStage ends then 0.0 else nan)
          # withLinkStrength (dynamic \ends -> if tieOf ends == Couple then 1.0 else 0.0)
          # withHardLimit true
      -- A couple one above the other, as the novel's own tree draws them:
      -- side by side, the in-law would push each generation further right.
      , directional "couples"
          # withAxis { x: 0.0, y: 1.0 }
          # withGap (dynamic coupleGap)
          # withLinkStrength (dynamic \ends -> if tieOf ends == Couple then 1.0 else 0.0)
          # withHardLimit true
      -- Every Buendía is drawn gently toward the same point on the left,
      -- and the hard limit stops each one a generation right of its parents:
      -- so each comes to rest as far left as its ancestry allows, which is a
      -- layering by generation that nobody placed. (Springs on the descent
      -- links fought the limit and dragged the tree to the right; a pull to
      -- the middle left the founders at the middle and the rest off stage.)
      , positionX "westward" # withX (static (stage.width / 2.0 - 3.0 * generationWidth)) # withStrength (static 0.05) # withFilter onStage
      , positionY "middle-y" # withY (static (stage.height / 2.0)) # withStrength (static 0.02) # withFilter onStage
      -- and nobody leaves the stage: couples stacked one under another, and a
      -- son's seventeen brothers, would otherwise hang off its foot.
      , containEach "stage" [ RectRegion { x0: margin, y0: margin, x1: stage.width - margin, y1: stage.height - margin } ]
          (\body -> if onStage body then Just 0 else Nothing)
          # withRadius (dynamic \body -> body.radius + 2.0)
      ]
    Generations -> confined _.generation
    GenerationsBySex -> confined _.cell
    _ ->
      [ link "meetings" # withDistance (static 40.0) # withLinkStrength (dynamic meetingStrength)
      , positionX "middle-x" # withX (static (stage.width / 2.0)) # withStrength (static 0.03) # withFilter onStage
      , positionY "middle-y" # withY (static (stage.height / 2.0)) # withStrength (static 0.06) # withFilter onStage
      ]

  confined :: (Body -> Maybe Int) -> Array (ForceConfig Body)
  confined landOf =
    [ link "meetings" # withDistance (static 30.0) # withLinkStrength (dynamic (betweenLands landOf))
    -- Soft and gentle, so that people walk into their lands. Hard (the
    -- default), anyone outside is put inside on the first tick, a leap of
    -- hundreds of pixels in a frame; soft, the push is a share of the distance
    -- still to go, so at a half it was still a leap for those furthest away.
    , containEach "lands" (region <$> landsOf atlas mode) landOf
        # withRadius (dynamic \body -> body.radius + 2.0)
        # withStrength (static 0.08)
        # withHardLimit false
    , cohesion "kin" landOf # withStrength (static 0.03)
    -- Whoever has no land in this partition waits below the middle.
    , positionX "unplaced-x" # withX (static (stage.width / 2.0)) # withStrength (dynamic \body -> unplaced landOf body)
    , positionY "unplaced-y" # withY (static (stage.height - 16.0)) # withStrength (dynamic \body -> unplaced landOf body)
    ]

  unplaced :: (Body -> Maybe Int) -> Body -> Number
  unplaced landOf body = case landOf body of
    Nothing -> 0.1
    Just _ -> 0.0

  betweenLands :: (Body -> Maybe Int) -> LinkEnds Body -> Number
  betweenLands landOf ends =
    (if landOf ends.source == landOf ends.target then 1.0 else 0.1) * meetingStrength ends

  -- d3's link strength, one over the smaller degree, for a meeting; nothing
  -- for the tree's ties outside the tree.
  meetingStrength :: LinkEnds Body -> Number
  meetingStrength ends = if not (bothOnStage ends) then 0.0 else case tieOf ends of
    Meeting -> 1.0 / toNumber (max 1 (min (degreeOf ends.source) (degreeOf ends.target)))
    _ -> 0.0

  tieOf :: LinkEnds Body -> Tie
  tieOf ends = fromMaybe Meeting (Array.index kinds ends.index)

  -- In the tree: a child at least a generation's width right of a parent; a
  -- couple side by side; meetings slack.
  -- No spring on descent: the westward pull and the hard limit place a
  -- child. Couples are held close.
  -- A tie acts only between two people on stage: a spring to someone still
  -- waiting at their corner would drag the one who has arrived off after them.
  bothOnStage ends = onStage ends.source && onStage ends.target
  treeLength ends = case tieOf ends of
    Descent -> generationWidth + 5.0
    Couple -> 26.0
    Meeting -> 30.0
  treeStrength ends = if not (bothOnStage ends) then 0.0 else case tieOf ends of
    Descent -> 0.0
    Couple -> 1.0
    Meeting -> 0.0
  descentGap ends = if not (bothOnStage ends) then nan else case tieOf ends of
    Descent -> generationWidth
    _ -> nan
  coupleGap ends = if not (bothOnStage ends) then nan else case tieOf ends of
    Couple -> 24.0
    _ -> nan

  degreeOf body = fromMaybe 1 (Array.index degrees body.id)

  region :: Land -> Region
  region land = case land.shape of
    Disc d -> CircleRegion d
    box -> PolygonRegion (outline box)

-- | Where a person waits before they arrive, just off the stage: the family on
-- | the left and everyone else on the right, men at the top and women at the
-- | bottom, which is where the last step will put them.
corner :: forall r. { person :: Person | r } -> { x :: Number, y :: Number }
corner body =
  { x: if body.person.family then -40.0 else stage.width + 40.0
  , y: if body.person.sex == Man then -40.0 else stage.height + 40.0
  }

-- | A place of one's own near one's corner: a small sunflower round it, so
-- | that no two people wait on the same point. (Where two coincide, a link
-- | between them has no direction, and even at strength 0 its NaN spreads.)
waiting :: Body -> { x :: Number, y :: Number }
waiting body =
  let
    home = corner body
    place = toNumber body.id
    angle = place * pi * (3.0 - sqrt 5.0)
    distance = 6.0 * sqrt (place + 1.0)
  in
    { x: home.x + distance * cos angle, y: home.y + distance * sin angle }

-- | The width of a generation in the tree: seven across, with room to spare.
generationWidth :: Number
generationWidth = 125.0

nan :: Number
nan = 0.0 / 0.0

-- | The outline of a land as the forces hold it and the paint starts from. A
-- | rectangle becomes the superellipse inside it (|x|^4 + |y|^4 = 1, a
-- | rounded square, and convex, as a polygon region must be), so that no one
-- | is held in a corner the wash does not reach.
outline :: Shape -> Array { x :: Number, y :: Number }
outline = case _ of
  Disc d -> Array.range 0 23 <#> \i ->
    let angle = 2.0 * pi * toNumber i / 24.0 in { x: d.x + d.radius * cos angle, y: d.y + d.radius * sin angle }
  Box b ->
    let
      cx = (b.x0 + b.x1) / 2.0
      cy = (b.y0 + b.y1) / 2.0
      rx = (b.x1 - b.x0) / 2.0
      ry = (b.y1 - b.y0) / 2.0
      rounded t = (if t < 0.0 then -1.0 else 1.0) * pow (abs t) 0.5
    in
      Array.range 0 (points - 1) <#> \i ->
        let angle = 2.0 * pi * toNumber i / toNumber points
        in { x: cx + rx * rounded (cos angle), y: cy + ry * rounded (sin angle) }
  where
  points = 24

-- | (`outline` gives 24 points for the forces; the paint takes every other
-- | one, `paintOutline`, since the wash deforms it anyway.)
paintOutline :: Shape -> Array { x :: Number, y :: Number }
paintOutline shape = Array.mapMaybe identity (Array.mapWithIndex (\i p -> if i `mod` 2 == 0 then Just p else Nothing) (outline shape))

-- | Whether a disc lies wholly inside a land's outline.
inside :: Shape -> { x :: Number, y :: Number, radius :: Number } -> Boolean
inside shape disc = case shape of
  Disc d -> sqrt ((disc.x - d.x) * (disc.x - d.x) + (disc.y - d.y) * (disc.y - d.y)) + disc.radius <= d.radius
  Box b ->
    let
      rx = (b.x1 - b.x0) / 2.0 - disc.radius
      ry = (b.y1 - b.y0) / 2.0 - disc.radius
      u = (disc.x - (b.x0 + b.x1) / 2.0) / rx
      v = (disc.y - (b.y0 + b.y1) / 2.0) / ry
    in
      -- a little slack: the polygon is a 24-sided approximation of the curve
      pow u 4.0 + pow v 4.0 <= 1.03
