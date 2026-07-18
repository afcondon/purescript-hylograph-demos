-- | The HATS canvas: the build DAG as a Sankey (the SystemSankey
-- | pattern from Minard), recolored live by the KB's beliefs, plus a
-- | shelf below for edge-less targets — the Sankey layout only knows
-- | nodes that links mention, and Ecosystem's phony constellation is
-- | mostly edge-less.
-- |
-- | Interaction split: hover stays entirely inside HATS coordinated
-- | highlighting (ancestry-aware — hovering a node lights the story of
-- | why it is what it is); only clicks cross the bridge to Halogen.
-- | Clicking a source touches it; clicking a target selects it for the
-- | proof card.
module Viz.Sankey
  ( NodeEvent(..)
  , SankeyInput
  , sankeyTree
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import DataViz.Layout.Sankey.Compute as Sankey
import DataViz.Layout.Sankey.Path as SankeyPath
import Effect (Effect)
import Hylograph.HATS (HighlightClass(..), Tree, elem, forEach, onClick, onCoordinatedHighlight, staticStr, thunkedStr, withBehaviors) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types as E
import Make.Model (BuildModel, Path(..), unPath)
import Make.Rules (BuildKB, TargetState(..), stateOf)
import Make.ToSankey (ancestryMap, isolatedPaths, sankeyLinks)

-- | Canvas → shell events.
data NodeEvent
  = TouchLeaf Path
  | SelectTarget Path

type SankeyInput =
  { width :: Number
  , height :: Number
  , model :: BuildModel
  , kb :: BuildKB
  , selected :: Maybe Path
  , notify :: NodeEvent -> Effect Unit
  }

-- | Swiss-restrained state palette (continuity with jtms-sudoku:
-- | amber is the same "gap" amber).
fillFor :: TargetState -> String
fillFor = case _ of
  SourceState -> "#f2f2f2"
  FreshState -> "#eef3ee"
  StaleState -> "#fff4e5"
  MissingState -> "#fdeeed"
  PhonyState -> "#ececf4"
  UnknownState -> "#f7f7f7"

strokeFor :: TargetState -> String
strokeFor = case _ of
  SourceState -> "#999999"
  FreshState -> "#7fa87f"
  StaleState -> "#e08b2d"
  MissingState -> "#d64541"
  PhonyState -> "#5b5ba6"
  UnknownState -> "#666666"

type NodeFlat =
  { name :: String
  , x0 :: Number
  , y0 :: Number
  , x1 :: Number
  , y1 :: Number
  , state :: TargetState
  }

type LinkFlat =
  { pathD :: String
  , sourceName :: String
  , targetName :: String
  , sourceX1 :: Number
  , targetX0 :: Number
  , sourceState :: TargetState
  , targetState :: TargetState
  }

sankeyTree :: SankeyInput -> HATS.Tree
sankeyTree input =
  HATS.elem E.SVG
    [ F.viewBox 0.0 0.0 (input.width + 20.0) (sankeyH + shelfHeight + 40.0)
    , F.preserveAspectRatio "xMidYMid meet"
    ]
    [ gradientDefs
    , HATS.elem E.Group [ F.transform "translate(10,10)" ]
        [ linksLayer <> nodesLayer <> labelsLayer <> shelfLayer ]
    ]
  where
  stateAt = stateOf input.kb

  linkRows = sankeyLinks input.model input.kb

  -- a link-poor diagram (Ecosystem: one edge) must not fill the canvas
  sankeyH = min input.height (max 90.0 (Int.toNumber (Array.length linkRows) * 90.0))

  layoutResult = Sankey.computeLayout linkRows input.width sankeyH

  nodeFlats :: Array NodeFlat
  nodeFlats = layoutResult.nodes <#> \n ->
    { name: n.name, x0: n.x0, y0: n.y0, x1: n.x1, y1: n.y1, state: stateAt (Path n.name) }

  linkFlats :: Array LinkFlat
  linkFlats = layoutResult.links <#> \link ->
    let
      srcNode = SankeyPath.findNode layoutResult.nodes link.sourceIndex
      tgtNode = SankeyPath.findNode layoutResult.nodes link.targetIndex
      srcName = fromMaybe "" (_.name <$> srcNode)
      tgtName = fromMaybe "" (_.name <$> tgtNode)
    in
      { pathD: SankeyPath.generateLinkPath layoutResult.nodes link
      , sourceName: srcName
      , targetName: tgtName
      , sourceX1: fromMaybe 0.0 (_.x1 <$> srcNode)
      , targetX0: fromMaybe 0.0 (_.x0 <$> tgtNode)
      , sourceState: stateAt (Path srcName)
      , targetState: stateAt (Path tgtName)
      }

  ancestry = ancestryMap input.model input.kb

  ancestryOf hoveredId = fromMaybe Set.empty (Map.lookup hoveredId ancestry)

  classifyNode :: String -> String -> HATS.HighlightClass
  classifyNode name hoveredId =
    if hoveredId == name then HATS.Primary
    else if Set.member name (ancestryOf hoveredId) then HATS.Related
    else HATS.Dimmed

  -- the isolated shelf, wrapped rows under the diagram
  isolated = isolatedPaths input.model

  perRow = max 1 (Int.floor (input.width / shelfCellW))
  shelfCellW = 150.0
  shelfRowH = 26.0
  shelfRows = (Array.length isolated + perRow - 1) / perRow
  shelfHeight = if Array.null isolated then 0.0 else Int.toNumber shelfRows * shelfRowH + 24.0

  shelfCells = Array.mapWithIndex
    ( \i p ->
        { name: unPath p
        , x: Int.toNumber (i `mod` perRow) * shelfCellW
        , y: sankeyH + 24.0 + Int.toNumber (i / perRow) * shelfRowH
        , state: stateAt p
        }
    )
    isolated

  clickEvent name state = case state of
    SourceState -> TouchLeaf (Path name)
    _ -> SelectTarget (Path name)

  isSelected name = input.selected == Just (Path name)

  gradientDefs = HATS.elem E.Defs []
    ( Array.mapWithIndex
        ( \i link ->
            HATS.elem E.LinearGradient
              [ HATS.staticStr "id" ("grad-" <> show i)
              , HATS.staticStr "gradientUnits" "userSpaceOnUse"
              , HATS.staticStr "x1" (show link.sourceX1)
              , HATS.staticStr "x2" (show link.targetX0)
              ]
              [ HATS.elem E.Stop
                  [ HATS.staticStr "offset" "0%"
                  , HATS.staticStr "stop-color" (strokeFor link.sourceState)
                  , HATS.staticStr "stop-opacity" "0.35"
                  ]
                  []
              , HATS.elem E.Stop
                  [ HATS.staticStr "offset" "100%"
                  , HATS.staticStr "stop-color" (strokeFor link.targetState)
                  , HATS.staticStr "stop-opacity" "0.35"
                  ]
                  []
              ]
        )
        linkFlats
    )

  linksLayer = HATS.forEach "links" E.Path linkFlats (\l -> l.sourceName <> "→" <> l.targetName) \link ->
    let
      linkId = link.sourceName <> "→" <> link.targetName
      linkIdx = fromMaybe 0
        (Array.findIndex (\l -> l.sourceName <> "→" <> l.targetName == linkId) linkFlats)
    in
      HATS.withBehaviors
        [ HATS.onCoordinatedHighlight
            { identify: linkId
            , classify: \hoveredId ->
                if hoveredId == linkId then HATS.Primary
                else if hoveredId == link.sourceName || hoveredId == link.targetName then HATS.Related
                else if
                  Set.member link.sourceName (ancestryOf hoveredId)
                    && Set.member link.targetName (ancestryOf hoveredId) then HATS.Related
                else HATS.Dimmed
            , group: Nothing
            }
        ] $
        HATS.elem E.Path
          [ F.d link.pathD
          , F.attr "fill" ("url(#grad-" <> show linkIdx <> ")")
          , F.stroke "none"
          , F.attr "class" "sankey-link"
          ]
          []

  nodesLayer = HATS.forEach "nodes" E.Rect nodeFlats _.name \node ->
    HATS.withBehaviors
      [ HATS.onClick (input.notify (clickEvent node.name node.state))
      , HATS.onCoordinatedHighlight
          { identify: node.name
          , classify: classifyNode node.name
          , group: Nothing
          }
      ] $
      HATS.elem E.Rect
        [ F.x node.x0
        , F.y node.y0
        , F.width (node.x1 - node.x0)
        , F.height (node.y1 - node.y0)
        , F.fill (fillFor node.state)
        , F.stroke (if isSelected node.name then "#111111" else strokeFor node.state)
        , F.strokeWidth (if isSelected node.name then 2.5 else 1.2)
        , F.attr "rx" "2"
        , F.attr "class" "sankey-node"
        , F.style "cursor: pointer"
        ]
        []

  labelsLayer = HATS.forEach "labels" E.Text nodeFlats _.name \node ->
    let
      onRight = node.x0 > input.width * 0.6
    in
      HATS.withBehaviors
        [ HATS.onCoordinatedHighlight
            { identify: node.name
            , classify: classifyNode node.name
            , group: Nothing
            }
        ] $
        HATS.elem E.Text
          [ HATS.thunkedStr "x" (show (if onRight then node.x0 - 4.0 else node.x1 + 4.0))
          , HATS.thunkedStr "y" (show (node.y0 + (node.y1 - node.y0) / 2.0))
          , HATS.staticStr "pointer-events" "none"
          , HATS.staticStr "text-anchor" (if onRight then "end" else "start")
          , HATS.staticStr "dominant-baseline" "middle"
          , HATS.staticStr "font-size" "10"
          , HATS.staticStr "font-weight" "500"
          , HATS.staticStr "fill" "#333"
          , HATS.staticStr "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
          , HATS.thunkedStr "textContent" node.name
          ]
          []

  shelfLayer =
    HATS.forEach "shelf-chips" E.Rect shelfCells _.name
      ( \cell ->
          HATS.withBehaviors
            [ HATS.onClick (input.notify (clickEvent cell.name cell.state))
            , HATS.onCoordinatedHighlight
                { identify: cell.name
                , classify: classifyNode cell.name
                , group: Nothing
                }
            ] $
            HATS.elem E.Rect
              [ F.x cell.x
              , F.y cell.y
              , F.width 10.0
              , F.height 10.0
              , F.fill (fillFor cell.state)
              , F.stroke (if isSelected cell.name then "#111111" else strokeFor cell.state)
              , F.strokeWidth (if isSelected cell.name then 2.0 else 1.0)
              , F.attr "rx" "2"
              , F.style "cursor: pointer"
              ]
              []
      )
      <> HATS.forEach "shelf-labels" E.Text shelfCells _.name
        ( \cell ->
            HATS.withBehaviors
              [ HATS.onClick (input.notify (clickEvent cell.name cell.state))
              , HATS.onCoordinatedHighlight
                  { identify: cell.name
                  , classify: classifyNode cell.name
                  , group: Nothing
                  }
              ] $
              HATS.elem E.Text
                [ HATS.thunkedStr "x" (show (cell.x + 15.0))
                , HATS.thunkedStr "y" (show (cell.y + 8.5))
                , HATS.staticStr "font-size" "10"
                , HATS.staticStr "fill" "#555"
                , HATS.staticStr "font-family" "-apple-system, 'Helvetica Neue', sans-serif"
                , HATS.staticStr "style" "cursor: pointer"
                , HATS.thunkedStr "textContent" cell.name
                ]
                []
        )
