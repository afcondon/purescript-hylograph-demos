-- | The picture, as functions from data to HATS trees.
module Territories.Chart
  ( Reading
  , nodesChart
  , controls
  , readout
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Element.Types (ElementType(..))
import Hylograph.HATS (Tree, elem, forEach, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F
import Territories.Geography (Geography, stage)
import Territories.Physics (Body, Mode(..), allModes, label)
import Territories.Story (Character(..), Story)

-- ============================================================================
-- The map and the characters
-- ============================================================================

-- | The territories and plazas, the meetings, the characters, and the names
-- | of the crossings. The map is in the picture in both modes; the page's
-- | style lets it fade in open country.
nodesChart :: Geography -> Story -> Mode -> Array Body -> Tree
nodesChart geography story mode bodies =
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height, F.class_ (modeClass mode) ]
    [ elem Group [ F.class_ "lands" ]
        [ forEach "territories" Circle geography.territories (show <<< _.index) \land ->
            elem Circle
              [ F.cx land.x
              , F.cy land.y
              , F.r land.radius
              , F.class_ (if land.bridge then "territory bridge" else "territory")
              , F.fill (colourOf geography (Just land.index))
              , F.attr "data-index" (show land.index)
              ]
              []
        , forEach "plazas" Circle geography.crossings (name <<< _.character) \crossing ->
            elem Circle [ F.cx crossing.x, F.cy crossing.y, F.r crossing.plaza, F.class_ "plaza" ] []
        ]
    , elem Group [ F.class_ "meetings" ]
        [ forEach "meetings" Line meetings _.key \meeting ->
            elem Line
              [ F.x1 meeting.from.x
              , F.y1 meeting.from.y
              , F.x2 meeting.to.x
              , F.y2 meeting.to.y
              , F.class_ (if meeting.bridge then "meeting bridge" else "meeting")
              ]
              []
        ]
    , elem Group [ F.class_ "characters" ]
        [ forEach "characters" Circle bodies (name <<< _.character) \body ->
            elem Circle
              [ F.cx body.x
              , F.cy body.y
              , F.r body.radius
              , F.class_ (if isJust body.post then "crossing" else "character")
              , F.fill (colourOf geography body.home)
              , F.attr "data-name" (name body.character)
              ]
              [ elem Title [ staticStr "textContent" (name body.character) ] [] ]
        ]
    , elem Group [ F.class_ "names" ]
        [ forEach "names" Text (Array.filter (isJust <<< _.post) bodies) (name <<< _.character) \body ->
            elem Text [ F.x (body.x + body.radius + 4.0), F.y (body.y - body.radius - 2.0), staticStr "textContent" (name body.character) ] []
        ]
    ]
  where
  byCharacter = Map.fromFoldable (bodies <#> \body -> Tuple body.character body)
  isBridge meeting = Array.any (\bridge -> bridge.source == meeting.source && bridge.target == meeting.target) geography.bridges
  meetings = story.meetings # Array.mapMaybe \meeting -> do
    source <- Map.lookup meeting.source byCharacter
    target <- Map.lookup meeting.target byCharacter
    pure
      { key: name meeting.source <> " - " <> name meeting.target
      , from: { x: source.x, y: source.y }
      , to: { x: target.x, y: target.y }
      , bridge: isBridge meeting
      }

name :: Character -> String
name (Character character) = character

modeClass :: Mode -> String
modeClass = case _ of
  Territories -> "map territories"
  OpenCountry -> "map open-country"

-- | One colour for each territory that is more than a bridge, largest first;
-- | slate for a bridge and whoever lives at the end of it; ink for a
-- | crossing, which has no territory of its own.
colourOf :: Geography -> Maybe Int -> String
colourOf geography = case _ of
  Nothing -> ink
  Just index -> fromMaybe slate (Array.elemIndex index proper >>= Array.index palette)
  where
  proper = _.index <$> Array.sortWith (negate <<< Array.length <<< _.members) (Array.filter (not <<< _.bridge) geography.territories)

ink :: String
ink = "#14181A"

slate :: String
slate = "#6B7378"

palette :: Array String
palette =
  [ "#14548C" -- blue
  , "#B8791F" -- ochre
  , "#2F7D5B" -- green
  , "#A81D22" -- red
  , "#6B4E9B" -- violet
  ]

-- ============================================================================
-- The buttons and the readout
-- ============================================================================

controls :: (Mode -> Effect Unit) -> Mode -> Tree
controls choose current =
  elem Div [ F.class_ "buttons" ]
    [ forEach "modes" Button allModes label \mode ->
        withBehaviors [ onClick (choose mode) ] $
          elem Button
            [ F.class_ (if mode == current then "button current" else "button")
            , F.attr "data-mode" (label mode)
            , staticStr "textContent" (label mode)
            ]
            []
    ]

type Reading =
  { mode :: Mode
  , ticks :: Int -- since the mode was chosen
  , maxDisplacement :: Number
  , maxAcceleration :: Number
  , settled :: Boolean
  }

readout :: Geography -> Reading -> Tree
readout geography reading =
  elem Div
    [ F.class_ "figures"
    , F.attr "data-settled" (show reading.settled)
    , F.attr "data-ticks" (show reading.ticks)
    , F.attr "data-mode" (label reading.mode)
    ]
    [ figure "territories" (show (Array.length geography.territories))
    , figure "crossings" (show (Array.length geography.crossings))
    , figure "bridges" (show (Array.length geography.bridges))
    , figure "furthest step" (toStringWith (fixed 3) reading.maxDisplacement)
    , figure "acceleration" (toStringWith (fixed 4) reading.maxAcceleration)
    , figure "ticks" (show reading.ticks)
    , figure "state" (if reading.settled then "at rest" else "walking")
    ]
  where
  figure caption value =
    elem Div [ F.class_ "figure" ]
      [ elem Span [ F.class_ "name", staticStr "textContent" caption ] []
      , elem Span [ F.class_ "value", staticStr "textContent" value ] []
      ]
