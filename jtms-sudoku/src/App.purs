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
import Story (SudokuFact, dagTreeFor, defaultDagFact, factTitle, gridTree, placementFor, proofSummary, skylineTree, stats)
import Sudoku.Board (Cell)
import Sudoku.Rules (SudokuClaim(..))

data Action = Initialize | CellClicked Cell

type State =
  { chosen :: SudokuFact
  , notify :: Maybe (Cell -> Effect Unit)
  }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const { chosen: defaultDagFact, notify: Nothing }
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
          [ HH.div_
              [ HH.h2_ [ HH.text "The grid, by tier" ]
              , HH.div [ HP.id "grid-view" ] []
              , HH.div [ HP.class_ (HH.ClassName "legend") ]
                  [ legend "#f2f2f2" "#999999" "given"
                  , legend "#e8f0f6" "#7aa6c2" "singles tier"
                  , legend "#fff4e5" "#e08b2d" "needs alldifferent (the gap)"
                  ]
              ]
          , HH.div_
              [ HH.h2_ [ HH.text "The derivation skyline" ]
              , HH.div [ HP.id "skyline-view" ] []
              , HH.div [ HP.class_ (HH.ClassName "legend") ]
                  [ HH.text "column height \x221d \x221a proof size \x2014 givens stub, singles low-rise, the gap towers" ]
              ]
          ]
      , HH.h2_ [ HH.text ("The proof: " <> factTitle state.chosen) ]
      , HH.p [ HP.class_ (HH.ClassName "legend") ]
          [ HH.text (proofLine (proofSummary state.chosen)) ]
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
      chosen <- H.gets _.chosen
      liftEffect (redraw notify chosen)
    CellClicked cell -> case placementFor cell of
      Nothing -> pure unit
      Just fact -> do
        H.modify_ _ { chosen = fact }
        state <- H.get
        case state.notify of
          Nothing -> pure unit
          Just notify -> liftEffect (redraw notify fact)

  -- static trees carry no join keys, so clear before rerender — the
  -- diff would otherwise merge stale nodes from the previous proof
  redraw notify fact = do
    clearContainer "#grid-view"
    clearContainer "#skyline-view"
    clearContainer "#dag-view"
    _ <- rerender "#grid-view" (gridTree notify (Just (cellOfFact fact)))
    _ <- rerender "#skyline-view" (skylineTree notify (Just (cellOfFact fact)))
    _ <- rerender "#dag-view" (dagTreeFor fact)
    attachPanZoom "#dag-view svg"
    pure unit

proofLine :: { shown :: Int, total :: Int } -> String
proofLine { total } =
  "the complete proof: " <> show total
    <> " facts \x2014 scroll to zoom, drag to pan"

cellOfFact :: SudokuFact -> Cell
cellOfFact f = case f.claim of
  Is c _ -> c
  Not c _ -> c
