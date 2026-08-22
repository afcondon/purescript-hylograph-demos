-- | Glassbox.Interpret
-- |
-- | One reading of a `Machine`: interpret it into a `Data.Machine.Mealy` from
-- | purescript-machines and run it.
-- |
-- | The Mealy carries the state and nothing else. Environment and configuration
-- | arrive with each event rather than being captured at construction, which is
-- | both honest — the grid and the converter's occupant genuinely change between
-- | presses — and what keeps the closure free of anything the diagram cannot see.
module Glassbox.Interpret
  ( Input
  , Step
  , toMealy
  , initialStep
  ) where

import Data.Machine.Mealy (Mealy, unfoldMealy)
import Data.Tuple (Tuple(..))
import Glassbox.Machine (Machine, Outcome(..), outcomeTarget)

-- | What the machine is fed: an event, plus the world as it stands.
type Input env cfg event =
  { env :: env
  , cfg :: cfg
  , event :: event
  }

-- | What the machine emits: where it was, what happened, where it now is.
-- |
-- | `outcome` is carried out rather than collapsed so that a refusal reaches the
-- | display instead of vanishing into an unchanged state.
type Step state event =
  { from :: state
  , event :: event
  , outcome :: Outcome state
  , current :: state
  }

-- | Interpret the machine into a Mealy.
toMealy
  :: forall env cfg state event deadline
   . Machine env cfg state event deadline
  -> Mealy (Input env cfg event) (Step state event)
toMealy machine = unfoldMealy machine.initial step
  where
  step from { env, cfg, event } =
    let
      outcome = machine.transition env cfg from event
      current = outcomeTarget from outcome
    in
      Tuple current { from, event, outcome, current }

-- | The step a machine is notionally in before anything has been fed to it.
-- |
-- | Reads `initial` from the same field `toMealy` seeds from, so the view has no
-- | second source for where the machine starts.
initialStep
  :: forall env cfg state event deadline
   . Machine env cfg state event deadline
  -> event
  -> Step state event
initialStep machine event =
  { from: machine.initial
  , event
  , outcome: Stay
  , current: machine.initial
  }
