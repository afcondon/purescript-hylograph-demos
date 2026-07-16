-- | The Halogen shell: headings and two containers HATS owns the insides
-- | of. All the substance is in `Story`.
module App (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hylograph.HATS.InterpreterTick (rerender)
import Story (dagTree, gridTree, stats)

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Initialize })
  }
  where
  render _ =
    HH.div_
      [ HH.h1_ [ HH.text "Explainable Sudoku" ]
      , HH.p [ HP.class_ (HH.ClassName "sub") ]
          [ HH.text $
              "purescript-jtms proofs rendered by hylograph. The oracle-discovered gap puzzle: "
                <> show stats.givens
                <> " givens, the singles tier earns "
                <> show stats.singles
                <> " more cells and stalls, and Régin's alldifferent earns the remaining "
                <> show stats.regin
                <> " — every one with a derivation."
          ]
      , HH.h2_ [ HH.text "The grid, by tier" ]
      , HH.div [ HP.id "grid-view" ] []
      , HH.div [ HP.class_ (HH.ClassName "legend") ]
          [ legend "#f2f2f2" "#999999" "given"
          , legend "#e8f0f6" "#7aa6c2" "singles tier"
          , legend "#fff4e5" "#e08b2d" "needs alldifferent (the gap)"
          ]
      , HH.h2_ [ HH.text "The smallest proof that needed the algorithm (Sugiyama layout)" ]
      , HH.div [ HP.id "dag-view" ] []
      , HH.div [ HP.class_ (HH.ClassName "legend") ]
          [ legend "#f2f2f2" "#999999" "given"
          , legend "#e8f0f6" "#7aa6c2" "sole digit"
          , legend "#eef3ee" "#7fa87f" "peer elimination"
          , legend "#fdeeed" "#d64541" "naked single"
          , legend "#ececf4" "#5b5ba6" "hidden single"
          , legend "#fff4e5" "#e08b2d" "alldifferent (Régin)"
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
    Initialize -> liftEffect do
      _ <- rerender "#grid-view" gridTree
      _ <- rerender "#dag-view" dagTree
      pure unit
