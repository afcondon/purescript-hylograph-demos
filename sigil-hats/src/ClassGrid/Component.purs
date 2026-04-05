-- | Halogen component for the PureScript type class hierarchy grid.
-- |
-- | 24 classes across 8 groups arranged in a 2×4 force-directed grid.
-- | Each group gets its own positionX/positionY target so nodes cluster
-- | into grid cells while maintaining internal hierarchy via link forces.
module ClassGrid.Component
  ( component
  ) where

import Prelude

import Data.Array (find, mapMaybe, (..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hylograph.HATS (Tree, elem, forEach, withBehaviors, onDrag, onZoom) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender) as HATS
import Hylograph.Internal.Behavior.FFI (registerSimulation_)
import Hylograph.Internal.Behavior.Types (ScaleExtent(..), ZoomConfig(..), simulationDragNested)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Simulation (runSimulation, Engine(..), SimulationHandle, SimulationEvent(..))
import Hylograph.Simulation.Emitter (SimulationEmitter)
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.ForceEngine.Setup (Setup, setup, manyBody, collide, link, positionX, positionY, withStrength, withRadius, withDistance, withX, withY, static, dynamic)
import Hylograph.ForceEngine.Simulation (SimulationNode)

import ClassGrid.Data (ClassInfo, allClasses, allEdges, groupCenterX, groupCenterY, groupLabel, groupColor)

-- =============================================================================
-- Types
-- =============================================================================

type ClassNodeRow = (group :: Int, cardW :: Number, cardH :: Number)
type ClassNode = SimulationNode ClassNodeRow

type EnrichedNode =
  { node :: ClassNode
  , info :: ClassInfo
  }

type EnrichedEdge =
  { sx :: Number, sy :: Number
  , tx :: Number, ty :: Number
  , key :: String
  , group :: Int
  }

data Action
  = Initialize
  | OnTick SimulationEvent

type State =
  { handle :: Maybe (SimulationHandle ClassNodeRow)
  }

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { handle: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.div [ HP.id "grid-container" ] []

-- =============================================================================
-- Action Handler
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect renderContainer
    result <- liftEffect createSimulation
    liftEffect $ registerSimulation_ simulationId result.handle.reheat
    H.modify_ _ { handle = Just result.handle }
    emitter <- liftEffect $ toHalogenEmitter result.events
    void $ H.subscribe $ emitter <#> OnTick

  OnTick (Tick _) -> do
    state <- H.get
    case state.handle of
      Nothing -> pure unit
      Just handle -> do
        nodes <- liftEffect handle.getNodes
        liftEffect $ renderGrid nodes

  OnTick _ -> pure unit

simulationId :: String
simulationId = "class-grid"

-- =============================================================================
-- SVG Container
-- =============================================================================

-- | ViewBox coordinate space (larger = more zoomed out).
vbWidth :: Number
vbWidth = 3600.0

vbHeight :: Number
vbHeight = 1800.0

renderContainer :: Effect Unit
renderContainer = do
  let labelsTree = groupLabels
      tree =
        HATS.withBehaviors
          [ HATS.onZoom (ZoomConfig { scaleExtent: ScaleExtent 0.3 5.0, targetSelector: ".zoom-group" }) ]
        $ HATS.elem SVG
          [ F.width 1400.0
          , F.height 700.0
          , F.viewBox (negate vbWidth / 2.0) (negate vbHeight / 2.0) vbWidth vbHeight
          , F.attr "id" "grid-svg"
          , F.preserveAspectRatio "xMidYMid meet"
          ]
          [ HATS.elem Group [ F.attr "class" "zoom-group" ]
              [ labelsTree
              , HATS.elem Group [ F.attr "id" "grid-edges" ] []
              , HATS.elem Group [ F.attr "id" "grid-nodes" ] []
              ]
          ]
  _ <- HATS.rerender "#grid-container" tree
  pure unit

-- | Static group labels at each cell center.
groupLabels :: HATS.Tree
groupLabels =
  HATS.forEach "labels" Text (0 .. 7) show \g ->
    HATS.elem Text
      [ F.attr "textContent" (groupLabel g)
      , F.x (groupCenterX g)
      , F.y (groupCenterY g - 200.0)
      , F.textAnchor "middle"
      , F.fontSize "15px"
      , F.fontWeight "500"
      , F.fontFamily "-apple-system, BlinkMacSystemFont, Helvetica Neue, sans-serif"
      , F.fill "#94a3b8"
      , F.attr "letter-spacing" "0.05em"
      ]
      []

-- =============================================================================
-- Force Simulation
-- =============================================================================

cardPad :: Number
cardPad = 14.0

gridSetup :: Setup ClassNode
gridSetup = setup "class-grid"
  [ manyBody "charge" # withStrength (static (-500.0))
  , collide "collide" # withRadius (dynamic \n -> max (n.cardW + cardPad * 2.0) (n.cardH + cardPad * 2.0) / 2.0 + 10.0)
  , link "links" # withDistance (static 120.0) # withStrength (static 0.4)
  , positionX "groupX" # withX (dynamic \n -> groupCenterX n.group) # withStrength (static 0.15)
  , positionY "groupY" # withY (dynamic \n -> groupCenterY n.group) # withStrength (static 0.15)
  ]

initialNodes :: Array ClassNode
initialNodes = allClasses <#> \c ->
  { id: c.id
  , x: groupCenterX c.group + toNumber ((c.id `mod` 5) - 2) * 40.0
  , y: groupCenterY c.group + toNumber ((c.id `mod` 3) - 1) * 40.0
  , vx: 0.0, vy: 0.0
  , fx: null, fy: null
  , group: c.group
  , cardW: c.width
  , cardH: c.height
  }

initialLinks :: Array { source :: Int, target :: Int }
initialLinks = allEdges

createSimulation :: Effect { handle :: SimulationHandle ClassNodeRow, events :: SimulationEmitter }
createSimulation =
  runSimulation
    { engine: D3
    , setup: gridSetup
    , nodes: initialNodes
    , links: initialLinks
    , container: "#grid-nodes"
    , alphaMin: 0.001
    }

-- =============================================================================
-- Rendering
-- =============================================================================

findClass :: Int -> Maybe ClassInfo
findClass id = find (\c -> c.id == id) allClasses

enrichNodes :: Array ClassNode -> Array EnrichedNode
enrichNodes nodes = mapMaybe
  (\node -> findClass node.id <#> \info -> { node, info })
  nodes

enrichEdges :: Array ClassNode -> Array EnrichedEdge
enrichEdges nodes = mapMaybe mkEdge allEdges
  where
  findNode id = find (\n -> n.id == id) nodes
  mkEdge edge = do
    src <- findNode edge.source
    tgt <- findNode edge.target
    pure
      { sx: src.x, sy: src.y
      , tx: tgt.x, ty: tgt.y
      , key: show edge.source <> "-" <> show edge.target
      , group: src.group
      }

renderGrid :: Array ClassNode -> Effect Unit
renderGrid nodes = do
  -- Edges
  let edges = enrichEdges nodes
      edgesTree = HATS.forEach "edges" Line edges _.key \e ->
        HATS.elem Line
          [ F.x1 e.sx, F.y1 e.sy
          , F.x2 e.tx, F.y2 e.ty
          , F.strokeWidth 1.5
          , F.stroke (groupColor e.group)
          , F.opacity "0.35"
          ]
          []
  _ <- HATS.rerender "#grid-edges" edgesTree

  -- Nodes
  let enriched = enrichNodes nodes
      nodesTree = HATS.forEach "nodes" Group enriched nodeKey \en ->
        let cw = en.info.width + cardPad * 2.0
            ch = en.info.height + cardPad * 2.0
        in HATS.withBehaviors
          [ HATS.onDrag (simulationDragNested simulationId) ]
        $ HATS.elem Group
          [ F.transform ("translate(" <> show en.node.x <> "," <> show en.node.y <> ")")
          , F.style "cursor: grab;"
          ]
          [ -- Tinted card background
            HATS.elem Rect
              [ F.x (negate cw / 2.0)
              , F.y (negate ch / 2.0)
              , F.width cw
              , F.height ch
              , F.attr "rx" "10"
              , F.fill (groupColor en.info.group)
              , F.fillOpacity "0.12"
              , F.stroke (groupColor en.info.group)
              , F.strokeWidth 1.5
              , F.strokeOpacity "0.5"
              ]
              []
          , HATS.elem Group
              [ F.transform ("translate(" <> show (negate en.info.width / 2.0) <> "," <> show (negate en.info.height / 2.0) <> ")") ]
              [ en.info.classDeclTree ]
          ]
  _ <- HATS.rerender "#grid-nodes" nodesTree
  pure unit
  where
  nodeKey :: EnrichedNode -> String
  nodeKey en = show en.node.id
