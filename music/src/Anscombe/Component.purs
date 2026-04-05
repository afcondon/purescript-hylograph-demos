-- | Anscombe's Quartet - Main Component
-- |
-- | A Tufte-inspired visualization demonstrating that four datasets with
-- | identical statistical properties can have completely different structures.
-- |
-- | Architecture:
-- | - Clean state machine: Idle ↔ Playing
-- | - Halogen subscriptions for timer-driven updates
-- | - psd3-music for Web Audio scheduling
-- | - Declarative SVG with Tufte minimal aesthetic
module Anscombe.Component where

import Prelude

import Anscombe.Audio as Audio
import Anscombe.Data (Dataset, allDatasets)
import Anscombe.Timer as Timer
import Anscombe.Types (PlayState(..), currentNoteFor, initialState, isDatasetPlaying)
import Data.Array (length)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toStringWith, fixed)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.Music.Internal.FFI (AudioContext, createAudioContext)

-- =============================================================================
-- Actions
-- =============================================================================

-- | Component actions form a clean state machine:
-- |
-- |   User: ToggleDataset → transitions Idle ↔ Playing
-- |   Timer: AdvanceNote → cycles noteIndex when Playing
-- |   Timer: ScheduleNextCycle → schedules next audio cycle
data Action
  = Initialize
  | ToggleDataset Dataset
  | AdvanceNote
  | ScheduleNextCycle

-- =============================================================================
-- Component State
-- =============================================================================

-- | Runtime state extends the logical state with subscription handles.
type ComponentState =
  { playState :: PlayState
  , audioCtx :: Maybe AudioContext
  , noteSubscription :: Maybe H.SubscriptionId
  , cycleSubscription :: Maybe H.SubscriptionId
  }

mkInitialState :: ComponentState
mkInitialState =
  { playState: initialState.playState
  , audioCtx: initialState.audioCtx
  , noteSubscription: Nothing
  , cycleSubscription: Nothing
  }

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> mkInitialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. ComponentState -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "anscombe-container") ]
    [ renderHeader
    , renderQuartet state
    , renderFooter
    ]

renderHeader :: forall m. H.ComponentHTML Action () m
renderHeader =
  HH.header
    [ HP.class_ (H.ClassName "header") ]
    [ HH.h1_ [ HH.text "Anscombe's Quartet" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Four datasets with identical statistical properties" ]
    , HH.p
        [ HP.class_ (H.ClassName "stats") ]
        [ HH.text "Mean of x: 9 | Mean of y: 7.50 | Correlation: 0.816 | Regression: y = 3.00 + 0.500x" ]
    ]

renderQuartet :: forall m. ComponentState -> H.ComponentHTML Action () m
renderQuartet state =
  HH.div
    [ HP.class_ (H.ClassName "quartet-grid") ]
    (allDatasets <#> renderDatasetCard state)

renderDatasetCard :: forall m. ComponentState -> Dataset -> H.ComponentHTML Action () m
renderDatasetCard state dataset =
  let
    isPlaying = isDatasetPlaying dataset.name state.playState
    highlightIdx = fromMaybe (-1) $ currentNoteFor dataset.name state.playState
  in
    HH.div
      [ HP.class_ (H.ClassName "dataset-card") ]
      [ renderScatterPlot dataset highlightIdx
      , HH.div
          [ HP.class_ (H.ClassName "controls") ]
          [ HH.button
              [ HP.class_ (H.ClassName if isPlaying then "stop-btn" else "play-btn")
              , HE.onClick \_ -> ToggleDataset dataset
              ]
              [ HH.text (if isPlaying then "Stop" else "Play") ]
          ]
      ]

renderScatterPlot :: forall m. Dataset -> Int -> H.ComponentHTML Action () m
renderScatterPlot dataset highlightIdx =
  let
    -- Tufte-style dimensions
    width = 280.0
    height = 280.0
    margin = { top: 30.0, right: 20.0, bottom: 40.0, left: 40.0 }
    plotWidth = width - margin.left - margin.right
    plotHeight = height - margin.top - margin.bottom

    -- Scale functions (x: 0-20, y: 0-14)
    scaleX x = margin.left + (x / 20.0) * plotWidth
    scaleY y = margin.top + plotHeight - (y / 14.0) * plotHeight
  in
    svgElement "svg"
      [ HP.attr (HH.AttrName "viewBox") ("0 0 " <> show width <> " " <> show height)
      , HP.attr (HH.AttrName "class") "scatter-plot"
      ]
      [ -- Roman numeral label
        svgElement "text"
          [ HP.attr (HH.AttrName "x") (fmt (margin.left + 15.0))
          , HP.attr (HH.AttrName "y") (fmt (margin.top + 20.0))
          , HP.attr (HH.AttrName "class") "numeral"
          ]
          [ HH.text dataset.numeral ]

      -- L-shaped axes (Tufte minimal)
      , svgElement "g" [ HP.attr (HH.AttrName "class") "axis" ]
          [ line (margin.left) (margin.top) (margin.left) (margin.top + plotHeight)
          , line (margin.left) (margin.top + plotHeight) (margin.left + plotWidth) (margin.top + plotHeight)
          , tick (scaleX 5.0) (margin.top + plotHeight) "5"
          , tick (scaleX 10.0) (margin.top + plotHeight) "10"
          , tick (scaleX 15.0) (margin.top + plotHeight) "15"
          , tick (scaleX 20.0) (margin.top + plotHeight) "20"
          , ytick (margin.left) (scaleY 5.0) "5"
          , ytick (margin.left) (scaleY 10.0) "10"
          ]

      -- Data points
      , svgElement "g" [ HP.attr (HH.AttrName "class") "points" ]
          (Array.mapWithIndex (renderPoint scaleX scaleY highlightIdx) dataset.points)
      ]

renderPoint
  :: forall m
   . (Number -> Number)
  -> (Number -> Number)
  -> Int
  -> Int
  -> { x :: Number, y :: Number }
  -> H.ComponentHTML Action () m
renderPoint scaleX scaleY highlightIdx idx point =
  let
    isHighlighted = idx == highlightIdx
  in
    svgElement "circle"
      [ HP.attr (HH.AttrName "cx") (fmt (scaleX point.x))
      , HP.attr (HH.AttrName "cy") (fmt (scaleY point.y))
      , HP.attr (HH.AttrName "r") (if isHighlighted then "6" else "4")
      , HP.attr (HH.AttrName "class") (if isHighlighted then "point highlight" else "point")
      ]
      []

renderFooter :: forall m. H.ComponentHTML Action () m
renderFooter =
  HH.footer
    [ HP.class_ (H.ClassName "footer") ]
    [ HH.p_
        [ HH.text "F. J. Anscombe, "
        , HH.em_ [ HH.text "Graphs in Statistical Analysis" ]
        , HH.text ", American Statistician, 27 (February 1973), 17-21."
        ]
    , HH.p
        [ HP.class_ (H.ClassName "tech") ]
        [ HH.text "Sonification: Y-axis maps to pitch. Click Play to hear each dataset." ]
    ]

-- =============================================================================
-- SVG Helpers
-- =============================================================================

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgElement :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace svgNS) (HH.ElemName name)

line :: forall w i. Number -> Number -> Number -> Number -> HH.HTML w i
line x1 y1 x2 y2 =
  svgElement "line"
    [ HP.attr (HH.AttrName "x1") (fmt x1)
    , HP.attr (HH.AttrName "y1") (fmt y1)
    , HP.attr (HH.AttrName "x2") (fmt x2)
    , HP.attr (HH.AttrName "y2") (fmt y2)
    ]
    []

tick :: forall w i. Number -> Number -> String -> HH.HTML w i
tick x y label =
  svgElement "g" []
    [ svgElement "line"
        [ HP.attr (HH.AttrName "x1") (fmt x)
        , HP.attr (HH.AttrName "y1") (fmt y)
        , HP.attr (HH.AttrName "x2") (fmt x)
        , HP.attr (HH.AttrName "y2") (fmt (y + 5.0))
        ]
        []
    , svgElement "text"
        [ HP.attr (HH.AttrName "x") (fmt x)
        , HP.attr (HH.AttrName "y") (fmt (y + 18.0))
        , HP.attr (HH.AttrName "class") "tick-label"
        ]
        [ HH.text label ]
    ]

ytick :: forall w i. Number -> Number -> String -> HH.HTML w i
ytick x y label =
  svgElement "g" []
    [ svgElement "line"
        [ HP.attr (HH.AttrName "x1") (fmt x)
        , HP.attr (HH.AttrName "y1") (fmt y)
        , HP.attr (HH.AttrName "x2") (fmt (x - 5.0))
        , HP.attr (HH.AttrName "y2") (fmt y)
        ]
        []
    , svgElement "text"
        [ HP.attr (HH.AttrName "x") (fmt (x - 10.0))
        , HP.attr (HH.AttrName "y") (fmt (y + 4.0))
        , HP.attr (HH.AttrName "class") "tick-label y-label"
        ]
        [ HH.text label ]
    ]

fmt :: Number -> String
fmt = toStringWith (fixed 1)

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM ComponentState Action () output m Unit
handleAction = case _ of
  Initialize ->
    pure unit

  ToggleDataset dataset -> do
    state <- H.get
    case state.playState of
      Playing ps | ps.dataset.name == dataset.name ->
        stopPlayback
      _ ->
        startPlayback dataset

  AdvanceNote -> do
    state <- H.get
    case state.playState of
      Idle -> pure unit
      Playing ps -> do
        let noteCount = length ps.dataset.points
        let nextIdx = (ps.noteIndex + 1) `mod` noteCount
        H.modify_ _ { playState = Playing ps { noteIndex = nextIdx } }

  ScheduleNextCycle -> do
    state <- H.get
    case state.playState, state.audioCtx of
      Playing ps, Just ctx -> do
        _ <- liftEffect $ Audio.scheduleDatasetCycle ctx ps.dataset
        pure unit
      _, _ ->
        pure unit

-- =============================================================================
-- Playback Control
-- =============================================================================

startPlayback :: forall output m. MonadAff m => Dataset -> H.HalogenM ComponentState Action () output m Unit
startPlayback dataset = do
  -- Stop any existing playback
  stopPlayback

  state <- H.get

  -- Create audio context if needed (must be in user gesture)
  ctx <- case state.audioCtx of
    Just c -> pure c
    Nothing -> liftEffect createAudioContext

  -- Schedule the first cycle of audio
  cycleDuration <- liftEffect $ Audio.scheduleDatasetCycle ctx dataset

  -- Subscribe to note advancement timer
  let noteIntervalMs = Audio.cycleParams.noteInterval * 1000.0
  noteSubId <- H.subscribe $ noteEmitter noteIntervalMs

  -- Subscribe to cycle scheduling timer
  cycleSubId <- H.subscribe $ cycleEmitter cycleDuration

  H.modify_ _
    { playState = Playing { dataset: dataset, noteIndex: 0 }
    , audioCtx = Just ctx
    , noteSubscription = Just noteSubId
    , cycleSubscription = Just cycleSubId
    }

stopPlayback :: forall output m. MonadAff m => H.HalogenM ComponentState Action () output m Unit
stopPlayback = do
  state <- H.get

  -- Unsubscribe from timers
  case state.noteSubscription of
    Just subId -> H.unsubscribe subId
    Nothing -> pure unit

  case state.cycleSubscription of
    Just subId -> H.unsubscribe subId
    Nothing -> pure unit

  H.modify_ _
    { playState = Idle
    , noteSubscription = Nothing
    , cycleSubscription = Nothing
    }

-- =============================================================================
-- Emitters (Timer-based subscriptions)
-- =============================================================================

-- | Emitter that fires AdvanceNote at regular intervals
noteEmitter :: Number -> HS.Emitter Action
noteEmitter intervalMs = HS.makeEmitter \emit -> do
  intervalId <- Timer.setInterval intervalMs (emit AdvanceNote)
  pure $ Timer.clearInterval intervalId

-- | Emitter that fires ScheduleNextCycle at cycle boundaries
cycleEmitter :: Number -> HS.Emitter Action
cycleEmitter cycleDurationMs = HS.makeEmitter \emit -> do
  intervalId <- Timer.setInterval cycleDurationMs (emit ScheduleNextCycle)
  pure $ Timer.clearInterval intervalId
