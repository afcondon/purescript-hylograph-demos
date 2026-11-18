-- | The picture, as functions from data to HATS trees.
module Labels.Chart
  ( Reading
  , Summary
  , summarise
  , nodesChart
  , controls
  , readout
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Hylograph.Element.Types (ElementType(..))
import Hylograph.HATS (Tree, elem, forEach, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F
import Labels.Engine (Kernel, allKernels, kernelLabel)
import Labels.Nations (Continent(..), Era, allEras, eraLabel)
import Labels.Physics (Body, Part(..), Placing, allPlacings, boxOf, placingLabel)
import Labels.Plot (across, frame, incomeTicks, leader, lifeTicks, overlapping, stage, up)

-- ============================================================================
-- The plot
-- ============================================================================

-- | A label is joined to its dot once it is further than this from the
-- | dot's edge.
leaderSlack :: Number
leaderSlack = 6.0

nodesChart :: Array Body -> Tree
nodesChart bodies =
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height ]
    [ elem Group [ F.class_ "axes" ]
        [ forEach "income-lines" Line incomeTicks show \income ->
            elem Line [ F.x1 (across income), F.x2 (across income), F.y1 frame.top, F.y2 frame.bottom ] []
        , forEach "life-lines" Line lifeTicks show \years ->
            elem Line [ F.x1 frame.left, F.x2 frame.right, F.y1 (up years), F.y2 (up years) ] []
        , forEach "income-ticks" Text incomeTicks show \income ->
            elem Text [ F.x (across income), F.y (frame.bottom + 16.0), F.class_ "tick across", staticStr "textContent" (dollars income) ] []
        , forEach "life-ticks" Text lifeTicks show \years ->
            elem Text [ F.x (frame.left - 8.0), F.y (up years), F.class_ "tick up", staticStr "textContent" (toStringWith (fixed 0) years) ] []
        , elem Text [ F.x frame.right, F.y (frame.bottom + 34.0), F.class_ "title across", staticStr "textContent" "income per person, dollars (logarithmic)" ] []
        , elem Text [ F.x (frame.left - 44.0), F.y (frame.top + 4.0), F.class_ "title up", staticStr "textContent" "life expectancy, years" ] []
        ]
    , elem Group [ F.class_ "leaders" ]
        [ forEach "leaders" Line leaders _.name \line ->
            elem Line [ F.x1 line.from.x, F.y1 line.from.y, F.x2 line.to.x, F.y2 line.to.y ] []
        ]
    , elem Group [ F.class_ "dots" ]
        [ forEach "dots" Circle dots _.name \dot ->
            elem Circle [ F.cx dot.x, F.cy dot.y, F.r dot.radius, F.fill (colourOf dot.continent), F.attr "data-name" dot.name ] []
        ]
    , elem Group [ F.class_ "labels" ]
        [ forEach "boxes" Rect labels _.name \label ->
            elem Rect
              [ F.x (label.x - label.halfWidth)
              , F.y (label.y - label.halfHeight)
              , F.width (2.0 * label.halfWidth)
              , F.height (2.0 * label.halfHeight)
              , F.attr "data-name" label.name
              ]
              []
        , forEach "names" Text labels _.name \label ->
            elem Text [ F.x label.x, F.y label.y, staticStr "textContent" label.name ] []
        ]
    ]
  where
  dots = Array.filter (\body -> body.part == ItsDot) bodies
  labels = Array.filter (\body -> body.part == ItsLabel) bodies
  leaders = labels # Array.mapMaybe \label ->
    leader leaderSlack { x: label.dot.x, y: label.dot.y, radius: label.radius } (boxOf label) <#> \line ->
      { name: label.name, from: line.from, to: line.to }

dollars :: Number -> String
dollars income =
  if income >= 1000.0 then "$" <> toStringWith (fixed 0) (income / 1000.0) <> "k"
  else "$" <> toStringWith (fixed 0) income

-- | One colour for each continent, for its dots. The names are all ink.
colourOf :: Continent -> String
colourOf = case _ of
  SubSaharanAfrica -> "#14548C"
  EuropeAndCentralAsia -> "#B8791F"
  America -> "#2F7D5B"
  EastAsiaAndPacific -> "#A81D22"
  MiddleEastAndNorthAfrica -> "#6B4E9B"
  SouthAsia -> "#1F8A99"

-- ============================================================================
-- What the picture amounts to
-- ============================================================================

type Summary =
  { nations :: Int
  , displaced :: Int -- labels with a leader
  , meanLeader :: Number -- over the displaced
  , maxLeader :: Number
  , overlaps :: Int -- pairs of labels that overlap, and labels over a dot
  }

summarise :: Array Body -> Summary
summarise bodies =
  { nations: Array.length labels
  , displaced: Array.length leaders
  , meanLeader: if Array.null leaders then 0.0 else sum leaders / toNumber (Array.length leaders)
  , maxLeader: fromMaybe 0.0 (maximum leaders)
  , overlaps: labelPairs + overDots
  }
  where
  dots = Array.filter (\body -> body.part == ItsDot) bodies
  labels = Array.filter (\body -> body.part == ItsLabel) bodies
  leaders = labels # Array.mapMaybe \label ->
    _.length <$> leader leaderSlack { x: label.dot.x, y: label.dot.y, radius: label.radius } (boxOf label)
  labelPairs = sum $ labels # Array.mapWithIndex \place label ->
    Array.length (Array.filter (overlapping (boxOf label) <<< boxOf) (Array.drop (place + 1) labels))
  overDots = sum $ labels <#> \label -> Array.length (Array.filter (covers label) dots)
  covers label dot =
    let
      nearestX = clamp (label.x - label.halfWidth) (label.x + label.halfWidth) dot.x
      nearestY = clamp (label.y - label.halfHeight) (label.y + label.halfHeight) dot.y
    in
      sqrt ((nearestX - dot.x) * (nearestX - dot.x) + (nearestY - dot.y) * (nearestY - dot.y)) < dot.radius

-- ============================================================================
-- The buttons and the readout
-- ============================================================================

controls
  :: { era :: Era -> Effect Unit, placing :: Placing -> Effect Unit, kernel :: Kernel -> Effect Unit }
  -> { era :: Era, placing :: Placing, kernel :: Kernel }
  -> Tree
controls choose current =
  elem Div [ F.class_ "controls" ]
    [ group "year" allEras eraLabel (_ == current.era) choose.era
    , group "labels" allPlacings placingLabel (_ == current.placing) choose.placing
    , group "kernel" allKernels kernelLabel (_ == current.kernel) choose.kernel
    ]
  where
  group :: forall choice. String -> Array choice -> (choice -> String) -> (choice -> Boolean) -> (choice -> Effect Unit) -> Tree
  group name choices label isCurrent press =
    elem Div [ F.class_ "buttons", F.attr "data-group" name ]
      [ forEach name Button choices label \choice ->
          withBehaviors [ onClick (press choice) ] $
            elem Button
              [ F.class_ (if isCurrent choice then "button current" else "button")
              , F.attr "data-choice" (label choice)
              , staticStr "textContent" (label choice)
              ]
              []
      ]

type Reading =
  { summary :: Summary
  , kernel :: Kernel
  , ticks :: Int -- since the last change
  , settled :: Boolean
  , millisecondsPerTick :: { pureScript :: Maybe Number, rust :: Maybe Number } -- mean over each kernel's last run
  }

readout :: Reading -> Tree
readout reading =
  elem Div
    [ F.class_ "figures"
    , F.attr "data-settled" (show reading.settled)
    , F.attr "data-ticks" (show reading.ticks)
    , F.attr "data-kernel" (kernelLabel reading.kernel)
    ]
    [ figure "nations" (show reading.summary.nations)
    , figure "overlaps" (show reading.summary.overlaps)
    , figure "leaders" (show reading.summary.displaced)
    , figure "mean leader" (toStringWith (fixed 1) reading.summary.meanLeader <> " px")
    , figure "longest" (toStringWith (fixed 1) reading.summary.maxLeader <> " px")
    , figure "PureScript" (perTick reading.millisecondsPerTick.pureScript)
    , figure "Rust" (perTick reading.millisecondsPerTick.rust)
    , figure "ticks" (show reading.ticks)
    , figure "state" (if reading.settled then "at rest" else "moving")
    ]
  where
  perTick = case _ of
    Just milliseconds -> toStringWith (fixed 2) milliseconds <> " ms/tick"
    Nothing -> "not run"
  figure caption value =
    elem Div [ F.class_ "figure", F.attr "data-figure" caption ]
      [ elem Span [ F.class_ "name", staticStr "textContent" caption ] []
      , elem Span [ F.class_ "value", staticStr "textContent" value ] []
      ]
