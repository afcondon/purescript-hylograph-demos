-- | The Halogen shell, canvas-first: the Sankey fills the window and
-- | the cards float above it. All rebuilding of belief is pure —
-- | touch produces a new snapshot, `buildKB` re-saturates, the canvas
-- | recolors. Hover never crosses this bridge (HATS coordinated
-- | highlighting owns it); only clicks do.
module App (component) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import Make.Model (BuildModel, Path, Snapshot, touch, unPath)
import Make.Proof (headline, proofLines, ultimately)
import Make.Rules (BuildKB, buildKB)
import Make.Scenarios (Scenario(..), loadScenario, scenarioCaption, scenarioLabel)
import PanZoom (attachPanZoom)
import Parsing (parseErrorMessage)
import Viz.Beliefs (beliefsTree)
import Viz.Sankey (NodeEvent(..), sankeyTree)

-- | Two projections of the same KB: data flowing through computations
-- | (the Sankey) vs the chain of beliefs that makes them happen or not
-- | (the derivation DAG).
data ViewMode = FlowView | BeliefView

derive instance Eq ViewMode

data Action
  = Initialize
  | FromCanvas NodeEvent
  | PickScenario Scenario
  | ResetSnapshot
  | SetView ViewMode

type World = { model :: BuildModel, snapshot :: Snapshot, kb :: BuildKB }

type State =
  { scenario :: Scenario
  , viewMode :: ViewMode
  , world :: Maybe World
  , parseError :: Maybe String
  , selected :: Maybe Path
  , notify :: Maybe (NodeEvent -> Effect Unit)
  }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const
      { scenario: BuildChain
      , viewMode: FlowView
      , world: Nothing
      , parseError: Nothing
      , selected: Nothing
      , notify: Nothing
      }
  , render
  , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Initialize })
  }
  where
  render state =
    HH.div_
      [ HH.div [ HP.id "sankey-view", HP.class_ (HH.ClassName "canvas") ] []
      , HH.div [ HP.class_ (HH.ClassName "overlay title-card") ]
          ( [ HH.h1_ [ HH.text "make, deconstructed" ]
            , HH.p [ HP.class_ (HH.ClassName "sub") ]
                [ HH.text
                    "A Makefile's semantics as monotone JTMS rules: staleness is a derived \
                    \belief with provenance, and this Sankey is a projection of the \
                    \derivation DAG. Click a grey source to touch it; click a target for \
                    \its proof; hover anything for its ancestry."
                ]
            , HH.p [ HP.class_ (HH.ClassName "sub") ]
                [ HH.text (scenarioCaption state.scenario) ]
            , HH.div [ HP.class_ (HH.ClassName "buttons") ]
                [ scenarioButton state BuildChain
                , scenarioButton state Ecosystem
                , HH.button [ HE.onClick \_ -> ResetSnapshot ] [ HH.text "reset snapshot" ]
                ]
            , HH.div [ HP.class_ (HH.ClassName "buttons") ]
                [ viewButton state FlowView "data flow"
                , viewButton state BeliefView "belief chain"
                ]
            ]
              <> case state.parseError of
                Nothing -> []
                Just err ->
                  [ HH.p [ HP.class_ (HH.ClassName "parse-error") ]
                      [ HH.text ("parse failed: " <> err) ]
                  ]
          )
      , HH.div [ HP.class_ (HH.ClassName "overlay legend-card") ]
          [ HH.h2_ [ HH.text "States" ]
          , HH.div [ HP.class_ (HH.ClassName "legend") ]
              [ legend "#f2f2f2" "#999999" "source"
              , legend "#eef3ee" "#7fa87f" "fresh"
              , legend "#fff4e5" "#e08b2d" "stale"
              , legend "#fdeeed" "#d64541" "missing"
              , legend "#ececf4" "#5b5ba6" "phony"
              ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "overlay proof-card") ] (proofCard state)
      ]

  scenarioButton state sc =
    HH.button
      [ HE.onClick \_ -> PickScenario sc
      , HP.classes
          ( [ HH.ClassName "scenario" ]
              <> if state.scenario == sc then [ HH.ClassName "active" ] else []
          )
      ]
      [ HH.text (scenarioLabel sc) ]

  viewButton state vm label =
    HH.button
      [ HE.onClick \_ -> SetView vm
      , HP.classes
          ( [ HH.ClassName "scenario" ]
              <> if state.viewMode == vm then [ HH.ClassName "active" ] else []
          )
      ]
      [ HH.text label ]

  legend fill stroke label =
    HH.span_
      [ HH.i [ HP.style ("background:" <> fill <> ";border:1px solid " <> stroke) ] []
      , HH.text label
      ]

  proofCard state = case state.world, state.selected of
    Just w, Just p ->
      [ HH.h2_ [ HH.text (unPath p) ]
      , HH.p [ HP.class_ (HH.ClassName "headline") ] [ HH.text (headline w.kb p) ]
      , HH.ul [ HP.class_ (HH.ClassName "proof") ]
          (proofLines w.kb p <#> \line -> HH.li_ [ HH.text line ])
      , HH.p [ HP.class_ (HH.ClassName "ultimately") ]
          [ HH.text (fromMaybe "" (ultimately w.kb p)) ]
      , recipeBlock w p
      ]
    _, _ ->
      [ HH.h2_ [ HH.text "Proof" ]
      , HH.p [ HP.class_ (HH.ClassName "sub") ]
          [ HH.text
              "Select a target to read why the engine believes what it believes \
              \about it — every line is a fact with a justification."
          ]
      ]

  recipeBlock w p = case Map.lookup p w.model.recipes of
    Nothing -> HH.text ""
    Just cmds ->
      HH.div_
        [ HH.h2_ [ HH.text "Recipe" ]
        , HH.pre [ HP.class_ (HH.ClassName "recipe") ]
            [ HH.text (Array.intercalate "\n" cmds) ]
        ]

  handleAction = case _ of
    Initialize -> do
      { emitter, listener } <- liftEffect HS.create
      void (H.subscribe (map FromCanvas emitter))
      H.modify_ _ { notify = Just (HS.notify listener) }
      loadInto BuildChain
      redraw
    PickScenario sc -> do
      loadInto sc
      redraw
    ResetSnapshot -> do
      st <- H.get
      loadInto st.scenario
      redraw
    FromCanvas (TouchLeaf p) -> do
      H.modify_ \st -> st
        { world = st.world <#> \w ->
            let
              snapshot' = touch p w.snapshot
            in
              { model: w.model, snapshot: snapshot', kb: buildKB w.model snapshot' }
        }
      redraw
    FromCanvas (SelectTarget p) -> do
      H.modify_ \st -> st
        { selected = if st.selected == Just p then Nothing else Just p }
      redraw
    SetView vm -> do
      H.modify_ _ { viewMode = vm }
      redraw

  loadInto sc = case loadScenario sc of
    Left err -> H.modify_ _
      { scenario = sc
      , world = Nothing
      , parseError = Just (parseErrorMessage err)
      , selected = Nothing
      }
    Right { model, snapshot } -> H.modify_ _
      { scenario = sc
      , world = Just { model, snapshot, kb: buildKB model snapshot }
      , parseError = Nothing
      , selected = Nothing
      }

  -- static trees carry no join keys, so clear before rerender — the
  -- diff would otherwise merge stale nodes from the previous state
  redraw = do
    state <- H.get
    case state.world, state.notify of
      Just w, Just notify -> liftEffect do
        clearContainer "#sankey-view"
        _ <- rerender "#sankey-view" case state.viewMode of
          FlowView -> sankeyTree
            { width: 1100.0
            , height: 560.0
            , model: w.model
            , kb: w.kb
            , selected: state.selected
            , notify
            }
          BeliefView -> beliefsTree
            { kb: w.kb
            , selected: state.selected
            , notify
            , sources: w.model.sources
            }
        attachPanZoom "#sankey-view svg"
      _, _ -> pure unit
