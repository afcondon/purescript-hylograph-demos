-- | The picture, as functions from data to HATS trees.
module Downhill.Chart
  ( Reading
  , nodesChart
  , controls
  , readout
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Downhill.Graph (Graph)
import Downhill.Physics (Body, Point, stage)
import Hylograph.Element.Types (ElementType(..))
import Hylograph.HATS (Tree, elem, forEach, onClick, onInput, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F

-- ============================================================================
-- The graph
-- ============================================================================

-- | The imports and the modules; behind them, when there is one, the ghost:
-- | where the first run had every module at the same tick.
nodesChart :: Graph -> (Int -> Effect Unit) -> { bodies :: Array Body, pinned :: Array Int, ghost :: Maybe (Array Point) } -> Tree
nodesChart graph press picture =
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height ]
    [ elem Group [ F.class_ "ghost" ]
        [ forEach "ghost-imports" Line (linesBetween ghostPlaces) _.key \line ->
            elem Line [ F.x1 line.from.x, F.y1 line.from.y, F.x2 line.to.x, F.y2 line.to.y ] []
        , forEach "ghost-modules" Rect ghosts _.name \ghost ->
            elem Rect
              [ F.x (ghost.x - ghost.halfWidth)
              , F.y (ghost.y - ghost.halfHeight)
              , F.width (2.0 * ghost.halfWidth)
              , F.height (2.0 * ghost.halfHeight)
              , F.attr "data-name" ghost.name
              , F.attr "data-x" (show ghost.x)
              , F.attr "data-y" (show ghost.y)
              ]
              []
        ]
    , elem Group [ F.class_ "imports" ]
        [ forEach "imports" Line (linesBetween (picture.bodies <#> \body -> { x: body.x, y: body.y })) _.key \line ->
            elem Line [ F.x1 line.from.x, F.y1 line.from.y, F.x2 line.to.x, F.y2 line.to.y ] []
        ]
    , elem Group [ F.class_ "modules" ]
        [ forEach "modules" Rect picture.bodies _.name \body ->
            withBehaviors [ onClick (press body.id) ] $
              elem Rect
                [ F.x (body.x - body.halfWidth)
                , F.y (body.y - body.halfHeight)
                , F.width (2.0 * body.halfWidth)
                , F.height (2.0 * body.halfHeight)
                , F.class_ (if Array.elem body.id picture.pinned then "module pinned" else "module")
                , F.stroke (colourOf body.family)
                , F.attr "data-name" body.name
                , F.attr "data-x" (show body.x)
                , F.attr "data-y" (show body.y)
                ]
                [ elem Title [ staticStr "textContent" body.name ] [] ]
        , forEach "names" Text picture.bodies _.name \body ->
            elem Text [ F.x body.x, F.y body.y, F.class_ (if Array.elem body.id picture.pinned then "pinned" else "free"), staticStr "textContent" body.label ] []
        ]
    ]
  where
  ghostPlaces = fromMaybe [] picture.ghost
  ghosts = Array.zipWith (\body place -> { name: body.name, x: place.x, y: place.y, halfWidth: body.halfWidth, halfHeight: body.halfHeight })
    picture.bodies
    ghostPlaces
  linesBetween places = graph.imports # Array.mapMaybe \needs -> do
    from <- Array.index places needs.source
    to <- Array.index places needs.target
    pure { key: show needs.source <> ">" <> show needs.target, from, to }

-- | A box is edged in its family's colour (the word after the library's in a
-- | module's name: HATS, Interpreter, Scale, ...).
colourOf :: Int -> String
colourOf family = fromMaybe "#4E555A" (Array.index palette (family `mod` Array.length palette))

palette :: Array String
palette =
  [ "#14548C"
  , "#A81D22"
  , "#2F7D5B"
  , "#B8791F"
  , "#6B4E9B"
  , "#1F8A99"
  , "#B5506E"
  , "#5E6B2E"
  , "#8A5A3C"
  , "#4E555A"
  ]

-- ============================================================================
-- The timeline's controls, and the readout
-- ============================================================================

type Reading =
  { tick :: Int
  , moments :: Int -- recorded so far
  , shown :: Int -- which of them is shown
  , playing :: Boolean
  , finished :: Boolean -- the run has cooled to its end
  , branched :: Boolean -- this run has left the first one
  , firstRunKept :: Boolean
  , pinned :: Int
  , alpha :: Number
  , downhill :: Int
  , imports :: Int
  }

controls :: { play :: Effect Unit, pause :: Effect Unit, scrub :: Int -> Effect Unit } -> Reading -> Tree
controls act reading =
  elem Div [ F.class_ "controls" ]
    [ withBehaviors [ onClick (if reading.playing then act.pause else act.play) ] $
        elem Button
          [ F.class_ "button current"
          , F.attr "id" "play"
          , staticStr "textContent" (if reading.playing then "Pause" else "Play from here")
          ]
          []
    , withBehaviors [ onInput \value -> case Int.fromString value of
        Just place -> act.scrub place
        Nothing -> pure unit ] $
        elem Input
          [ F.attr "type" "range"
          , F.attr "id" "scrub"
          , F.attr "min" "0"
          , F.attr "max" (show (reading.moments - 1))
          , F.attr "step" "1"
          , F.attr "value" (show reading.shown)
          , F.attr "aria-label" "moment in the run"
          ]
          []
    , elem Span
        [ F.class_ "hint"
        , staticStr "textContent"
            ( if reading.firstRunKept then "Drag the slider to go back. Click a module to pin it there (or let it go), then play: the first run stays behind as a ghost."
              else "The first run is being recorded."
            )
        ]
        []
    ]

readout :: Reading -> Tree
readout reading =
  elem Div
    [ F.class_ "figures"
    , F.attr "data-tick" (show reading.tick)
    , F.attr "data-moments" (show reading.moments)
    , F.attr "data-shown" (show reading.shown)
    , F.attr "data-playing" (show reading.playing)
    , F.attr "data-finished" (show reading.finished)
    , F.attr "data-branched" (show reading.branched)
    , F.attr "data-first-run-kept" (show reading.firstRunKept)
    , F.attr "data-downhill" (show reading.downhill)
    ]
    [ figure "tick" (show reading.tick)
    , figure "moment" (show (reading.shown + 1) <> " of " <> show reading.moments)
    , figure "alpha" (toStringWith (fixed 3) reading.alpha)
    , figure "pointing down" (show reading.downhill <> " of " <> show reading.imports)
    , figure "pinned" (show reading.pinned)
    , figure "run" (if reading.branched then "a different future" else "the first run")
    , figure "state" (if reading.playing then "running" else if reading.finished then "at rest" else "paused")
    ]
  where
  figure caption value =
    elem Div [ F.class_ "figure" ]
      [ elem Span [ F.class_ "name", staticStr "textContent" caption ] []
      , elem Span [ F.class_ "value", staticStr "textContent" value ] []
      ]
