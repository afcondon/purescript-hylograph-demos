-- | The Halogen shell: headings, two containers HATS owns the insides of,
-- | and the click bridge — HATS behaviors notify a Halogen subscription,
-- | Halogen re-renders the DAG panel for the chosen cell.
module App (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import PanZoom (attachPanZoom)
import Halogen.HTML.Events as HE
import Story (SudokuFact, dagTreeFor, defaultDagFact, factTitle, globalDagTree, gridTree, placementFor, proofSummary, skylineTree, stats)
import Sudoku.Board (Cell)
import Sudoku.Rules (SudokuClaim(..))

data Action = Initialize | CellClicked Cell | TogglePruned

type State =
  { chosen :: SudokuFact
  , pruned :: Boolean
  , notify :: Maybe (Cell -> Effect Unit)
  }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const { chosen: defaultDagFact, pruned: false, notify: Nothing }
  , render
  , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Initialize })
  }
  where
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Explainable Sudoku" ]
      , HH.p [ HP.class_ (HH.ClassName "sub") ]
          [ HH.text $
              "purescript-jtms proofs rendered by hylograph. The oracle-discovered gap puzzle: "
                <> show stats.givens
                <> " givens, the singles tier earns "
                <> show stats.singles
                <> " more cells and stalls, and R\xe9gin's alldifferent earns the remaining "
                <> show stats.regin
                <> " \x2014 every one with a derivation. Click any cell for its proof."
          ]
      , HH.div [ HP.class_ (HH.ClassName "row") ]
          [ HH.div [ HP.class_ (HH.ClassName "panel") ]
              [ HH.h2_ [ HH.text "The grid, by tier" ]
              , HH.div [ HP.id "grid-view" ] []
              , HH.div [ HP.class_ (HH.ClassName "legend") ]
                  [ legend "#f2f2f2" "#999999" "given"
                  , legend "#e8f0f6" "#7aa6c2" "singles tier"
                  , legend "#fff4e5" "#e08b2d" "needs alldifferent (the gap)"
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "panel") ]
              [ HH.h2_ [ HH.text "The derivation skyline" ]
              , HH.div [ HP.id "skyline-view" ] []
              , HH.div [ HP.class_ (HH.ClassName "legend") ]
                  [ HH.text "column height \x221d \x221a proof size \x2014 givens stub, singles low-rise, the gap towers" ]
              ]
          ]
      , HH.h2_ [ HH.text ("The solution graph \x2014 lit: " <> factTitle state.chosen) ]
      , HH.p [ HP.class_ (HH.ClassName "legend") ]
          [ HH.text (proofLine state.pruned (proofSummary state.chosen))
          , HH.text "  "
          , HH.button
              [ HE.onClick \_ -> TogglePruned ]
              [ HH.text (if state.pruned then "show the whole solution" else "prune to this proof") ]
          ]
      , HH.div [ HP.id "dag-view" ] []
      , HH.div [ HP.class_ (HH.ClassName "legend") ]
          [ legend "#f2f2f2" "#999999" "given"
          , legend "#e8f0f6" "#7aa6c2" "sole digit"
          , legend "#eef3ee" "#7fa87f" "peer elimination"
          , legend "#fdeeed" "#d64541" "naked single"
          , legend "#ececf4" "#5b5ba6" "hidden single"
          , legend "#fff4e5" "#e08b2d" "alldifferent (R\xe9gin)"
          , legend "#f7f7f7" "#666666" "mixed provenance"
          ]
      ]

  legend fill stroke label =
    HH.span_
      [ HH.i
          [ HP.style ("background:" <> fill <> ";border:1px solid " <> stroke) ]
          []
      , HH.text label
      ]

  handleAction = case _ of
    Initialize -> do
      { emitter, listener } <- liftEffect HS.create
      void (H.subscribe (map CellClicked emitter))
      let notify = HS.notify listener
      H.modify_ _ { notify = Just notify }
      state <- H.get
      liftEffect (redrawWith state.pruned notify state.chosen)
    CellClicked cell -> case placementFor cell of
      Nothing -> pure unit
      Just fact -> do
        H.modify_ _ { chosen = fact }
        redrawFromState
    TogglePruned -> do
      H.modify_ \st -> st { pruned = not st.pruned }
      redrawFromState

  redrawFromState = do
    state <- H.get
    case state.notify of
      Nothing -> pure unit
      Just notify -> liftEffect (redrawWith state.pruned notify state.chosen)

  -- static trees carry no join keys, so clear before rerender — the
  -- diff would otherwise merge stale nodes from the previous proof
  redrawWith pruned notify fact = do
    clearContainer "#grid-view"
    clearContainer "#skyline-view"
    clearContainer "#dag-view"
    _ <- rerender "#grid-view" (gridTree notify (Just (cellOfFact fact)))
    _ <- rerender "#skyline-view" (skylineTree notify (Just (cellOfFact fact)))
    _ <- rerender "#dag-view"
      (if pruned then dagTreeFor fact else globalDagTree (Just fact))
    attachPanZoom "#dag-view svg"
    pure unit

proofLine :: Boolean -> { cone :: Int, total :: Int } -> String
proofLine pruned { cone, total }
  | pruned =
      "pruned to this proof: " <> show cone
        <> " facts \x2014 scroll to zoom, drag to pan"
  | otherwise =
      "the whole solution: " <> show total <> " facts, this proof's "
        <> show cone
        <> " lit \x2014 scroll to zoom, drag to pan"

cellOfFact :: SudokuFact -> Cell
cellOfFact f = case f.claim of
  Is c _ -> c
  Not c _ -> c
