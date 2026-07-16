-- | The Halogen shell, canvas-first: the solution graph fills the window,
-- | and the board, city, and captions float above it as cards. All the
-- | substance is in `Story`.
module App (component) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import PanZoom (attachPanZoom)
import Story (SudokuFact, dagTreeFor, factTitle, globalDagTree, gridTree, placementFor, proofSummary, skylineTree, stats)
import Sudoku.Board (Cell)
import Sudoku.Rules (SudokuClaim(..))

data Action = Initialize | CellClicked Cell | TogglePruned

type State =
  { chosen :: Maybe SudokuFact
  , pruned :: Boolean
  , notify :: Maybe (Cell -> Effect Unit)
  }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const { chosen: Nothing, pruned: false, notify: Nothing }
  , render
  , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Initialize })
  }
  where
  render state =
    HH.div_
      [ HH.div [ HP.id "dag-view", HP.class_ (HH.ClassName "canvas") ] []
      , HH.div [ HP.class_ (HH.ClassName "overlay title-card") ]
          [ HH.h1_ [ HH.text "Explainable Sudoku" ]
          , HH.p [ HP.class_ (HH.ClassName "sub") ]
              [ HH.text $
                  "purescript-jtms proofs rendered by hylograph. Behind these cards: the whole solution, one graph. "
                    <> show stats.givens
                    <> " givens; the singles tier earns "
                    <> show stats.singles
                    <> " cells and stalls; R\xe9gin's alldifferent earns the remaining "
                    <> show stats.regin
                    <> ". Click any cell to light its proof."
              ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "overlay grid-card") ]
          [ HH.h2_ [ HH.text "The grid, by tier" ]
          , HH.div [ HP.id "grid-view" ] []
          , HH.div [ HP.class_ (HH.ClassName "legend") ]
              [ legend "#f2f2f2" "#999999" "given"
              , legend "#e8f0f6" "#7aa6c2" "singles"
              , legend "#fff4e5" "#e08b2d" "the gap"
              ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "overlay city-card") ]
          [ HH.h2_ [ HH.text "The derivation city" ]
          , HH.div [ HP.id "skyline-view" ] []
          , HH.div [ HP.class_ (HH.ClassName "legend") ]
              [ HH.text "height \x221d \x221a proof size" ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "overlay proof-card") ]
          [ HH.h2_
              [ HH.text case state.chosen of
                  Nothing -> "The solution graph"
                  Just f -> "Lit: " <> factTitle f
              ]
          , HH.p [ HP.class_ (HH.ClassName "legend") ]
              ( [ HH.text (proofLine state.pruned (proofSummary <$> state.chosen)) ]
                  <>
                    if isJust state.chosen then
                      [ HH.button
                          [ HE.onClick \_ -> TogglePruned ]
                          [ HH.text (if state.pruned then "show the whole solution" else "prune to this proof") ]
                      ]
                    else []
              )
          , HH.div [ HP.class_ (HH.ClassName "legend") ]
              [ legend "#f2f2f2" "#999999" "given"
              , legend "#e8f0f6" "#7aa6c2" "sole digit"
              , legend "#eef3ee" "#7fa87f" "peer elim"
              , legend "#fdeeed" "#d64541" "naked single"
              , legend "#ececf4" "#5b5ba6" "hidden single"
              , legend "#fff4e5" "#e08b2d" "alldifferent"
              , legend "#f7f7f7" "#666666" "mixed"
              ]
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
      redrawFromState
    CellClicked cell -> case placementFor cell of
      Nothing -> pure unit
      Just fact -> do
        H.modify_ _ { chosen = Just fact }
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
  redrawWith pruned notify chosen = do
    clearContainer "#grid-view"
    clearContainer "#skyline-view"
    clearContainer "#dag-view"
    let selectedCell = cellOfFact <$> chosen
    _ <- rerender "#grid-view" (gridTree notify selectedCell)
    _ <- rerender "#skyline-view" (skylineTree notify selectedCell)
    _ <- rerender "#dag-view" case chosen of
      Nothing -> globalDagTree Nothing
      Just fact -> if pruned then dagTreeFor fact else globalDagTree (Just fact)
    attachPanZoom "#dag-view svg"
    pure unit

proofLine :: Boolean -> Maybe { cone :: Int, total :: Int } -> String
proofLine pruned = case _ of
  Nothing -> "the whole solution, fully lit \x2014 scroll to zoom, drag to pan"
  Just { cone, total }
    | pruned -> "pruned to this proof: " <> show cone <> " facts"
    | otherwise ->
        "this proof's " <> show cone <> " of " <> show total
          <> " facts lit \x2014 scroll to zoom, drag to pan"

cellOfFact :: SudokuFact -> Cell
cellOfFact f = case f.claim of
  Is c _ -> c
  Not c _ -> c
