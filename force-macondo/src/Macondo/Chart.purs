-- | The picture, as functions from data to HATS trees.
module Macondo.Chart
  ( Reading
  , passage
  , chart
  , controls
  , readout
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Number (sqrt)
import Data.Int (toNumber)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Element.Types (ElementType(..))
import Hylograph.HATS (Tree, elem, forEach, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F
import Control.Alternative (guard)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Macondo.Script (Line(..), Word)
import Macondo.Physics (Body, Geography, Mode(..), Shape(..), allModes, label, landsOf, stage)
import Macondo.Story (Story)
import Macondo.Wash (hueColour)

-- | The lands of the current mode and their captions, the meetings, the
-- | people, and the names of those present in at least `named` passages.
chart :: Geography -> Story -> Mode -> Set Int -> Set Int -> Array Body -> Tree
chart atlas story mode seen arrived bodies =
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height, F.class_ (modeClass mode) ]
    [ elem Group [ F.class_ "lands" ]
        [ forEach "discs" Circle discs _.name \land ->
            elem Circle [ F.cx land.x, F.cy land.y, F.r land.radius, F.class_ "land" ] []
        , forEach "boxes" Rect boxes _.name \land ->
            elem Rect [ F.x land.x0, F.y land.y0, F.width (land.x1 - land.x0), F.height (land.y1 - land.y0), F.class_ "land" ] []
        , forEach "captions" Text lands _.name \land ->
            elem Text [ F.x land.caption.x, F.y land.caption.y, F.class_ "land-name", staticStr "textContent" land.name ] []
        ]
    , elem Group [ F.class_ "ties" ]
        [ forEach "ties" Line treeTies _.key \tie ->
            elem Line [ F.x1 tie.from.x, F.y1 tie.from.y, F.x2 tie.to.x, F.y2 tie.to.y, F.class_ tie.kind ] []
        ]
    , elem Group [ F.class_ "meetings" ]
        [ forEach "meetings" Line meetings _.key \meeting ->
            elem Line
              [ F.x1 meeting.from.x
              , F.y1 meeting.from.y
              , F.x2 meeting.to.x
              , F.y2 meeting.to.y
              , F.attr "stroke-width" (toStringWith (fixed 2) meeting.width)
              , F.class_ "meeting"
              ]
              []
        ]
    , elem Group [ F.class_ "people" ]
        [ forEach "people" Circle bodies (_.id <<< _.person) \body ->
            elem Circle
              [ F.cx body.x
              , F.cy body.y
              , F.r body.radius
              , F.class_ (personClass body)
              , F.style (discColour mode body)
              , F.attr "data-id" body.person.id
              ]
              [ elem Title [ staticStr "textContent" (body.person.name <> " · " <> show body.person.appearances <> " passages") ] [] ]
        ]
    , elem Group [ F.class_ "names" ]
        [ forEach "names" Text (Array.filter isNamed bodies) (_.id <<< _.person) \body ->
            elem Text [ F.x (body.x + body.radius + 3.0), F.y (body.y + 4.0), staticStr "textContent" body.person.name ] []
        ]
    ]
  where
  lands = landsOf atlas mode
  discs = lands # Array.mapMaybe \land -> case land.shape of
    Disc d -> Just { name: land.name, x: d.x, y: d.y, radius: d.radius }
    Box _ -> Nothing
  boxes = lands # Array.mapMaybe \land -> case land.shape of
    Box b -> Just { name: land.name, x0: b.x0, y0: b.y0, x1: b.x1, y1: b.y1 }
    Disc _ -> Nothing
  byId = Map.fromFoldable (bodies <#> \body -> Tuple body.person.id body)
  inTree = mode == Tree
  -- In the tree only the Buendías who have arrived are seen; after it,
  -- everyone.
  visible body = Set.member body.id seen && (not inTree || body.person.family)
  -- A tie is drawn once both ends have landed.
  landed body = Set.member body.id arrived
  -- In the tree every Buendía is named, as on the novel's own tree; after
  -- it, only those present in `named` passages or more.
  isNamed body = visible body && (if inTree then body.person.family else body.person.appearances >= named)
  personClass body
    | not (visible body) = "person offstage"
    | body.person.family = "person family"
    | otherwise = "person"
  -- The tree's ties: parent to child, and couples. Drawn only in the tree.
  treeTies = if not inTree then [] else descent <> couples
  descent = Array.filter landed bodies # Array.concatMap \child -> child.person.parents # Array.mapMaybe \parent -> do
    from <- Map.lookup parent byId
    guard (landed from)
    pure { key: parent <> " > " <> child.person.id, from: { x: from.x, y: from.y }, to: { x: child.x, y: child.y }, kind: "descent" }
  couples = story.couples # Array.mapMaybe \couple -> do
    a <- Map.lookup couple.a byId
    b <- Map.lookup couple.b byId
    guard (landed a && landed b)
    pure { key: couple.a <> " & " <> couple.b, from: { x: a.x, y: a.y }, to: { x: b.x, y: b.y }, kind: "couple" }
  meetings = if inTree then [] else story.meetings # Array.mapMaybe \meeting -> do
    source <- Map.lookup meeting.source byId
    target <- Map.lookup meeting.target byId
    guard (landed source && landed target)
    pure
      { key: meeting.source <> " - " <> meeting.target
      , from: { x: source.x, y: source.y }
      , to: { x: target.x, y: target.y }
      , width: 0.4 + 0.3 * sqrt (toNumber meeting.passages)
      }

-- | In black and white a Buendía is ink and anyone else paper; in colour the
-- | disc wears its generation, filled for a Buendía and as a ring otherwise.
discColour :: Mode -> Body -> String
discColour mode body = case mode of
  Tree -> ""
  OpenCountry -> ""
  _
    | body.person.family -> "fill:" <> colour <> ";stroke:#FBFCFA"
    | otherwise -> "fill:#FBFCFA;stroke:" <> colour <> ";stroke-width:1.6"
  where
  colour = hueColour (Just (body.person.era - 1))

-- | People present in this many passages or more are named on the picture;
-- | everyone is named on hover.
named :: Int
named = 25

modeClass :: Mode -> String
modeClass = case _ of
  Tree -> "map tree open-country"
  OpenCountry -> "map open-country"
  _ -> "map"

controls :: (Mode -> Effect Unit) -> Effect Unit -> Mode -> Tree
controls choose replay current =
  elem Div [ F.class_ "buttons" ]
    [ forEach "modes" Button allModes label \mode ->
        withBehaviors [ onClick (choose mode) ] $
          elem Button
            [ F.class_ (if mode == current then "button current" else "button")
            , F.attr "data-mode" (label mode)
            , staticStr "textContent" (label mode)
            ]
            []
    , withBehaviors [ onClick replay ] $
        elem Button [ F.class_ "button replay", F.attr "data-mode" "replay", staticStr "textContent" "Replay" ] []
    ]

-- | A passage of the novel as a run of words, each faded in as far as the
-- | script has got; a word that has not begun keeps its place, unseen, so
-- | the lines are set once and never reflow.
passage :: Array Word -> Tree
passage words =
  elem Div [ F.class_ "passage" ]
    [ line Banner, line Name, line Remembering, line Ending ]
  where
  line which =
    let
      these = Array.filter (\w -> w.line == which) words
    in
      if Array.null these then elem Span [ F.class_ "empty" ] []
      else elem Div [ F.class_ ("line " <> lineClass which) ]
        ( these <#> \w ->
            elem Span [ F.class_ "word", F.style ("opacity:" <> toStringWith (fixed 3) w.shown) , staticStr "textContent" (w.text <> " ") ] []
        )
  lineClass = case _ of
    Banner -> "banner"
    Name -> "name"
    Remembering -> "remembering"
    Ending -> "ending"

type Reading =
  { mode :: Mode
  , ticks :: Int -- since the mode was chosen
  , alpha :: Number
  , outside :: Int
  }

readout :: Story -> Reading -> Tree
readout story reading =
  elem Div
    [ F.class_ "figures"
    , F.attr "data-ticks" (show reading.ticks)
    , F.attr "data-mode" (label reading.mode)
    , F.attr "data-outside" (show reading.outside)
    ]
    [ figure "people" (show (Array.length story.people))
    , figure "family" (show (Array.length (Array.filter _.family story.people)))
    , figure "meetings" (show (Array.length story.meetings))
    , figure "outside their land" (show reading.outside)
    , figure "alpha" (toStringWith (fixed 3) reading.alpha)
    , figure "ticks" (show reading.ticks)
    ]
  where
  figure name value =
    elem Div [ F.class_ "figure" ]
      [ elem Span [ F.class_ "name", staticStr "textContent" name ] []
      , elem Span [ F.class_ "value", staticStr "textContent" value ] []
      ]
