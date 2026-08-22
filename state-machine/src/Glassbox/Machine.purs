-- | Glassbox.Machine
-- |
-- | A state machine as a **plain value** rather than a closure.
-- |
-- | This is the whole point of the library. `Data.Machine.Mealy` can already
-- | run a machine perfectly well, but `Mealy i o = Mealy (i -> Tuple (Mealy i o) o)`
-- | hides the state inside a closure, so a running Mealy can never say what
-- | states it has. A value that enumerates its states and events, and exposes
-- | its transition function, can be *interpreted* into a Mealy (see
-- | `Glassbox.Interpret`) and *rendered* into a diagram (see `Glassbox.Describe`)
-- | — and because both readings come from the same value, the thing that runs
-- | and the thing on screen cannot diverge.
module Glassbox.Machine
  ( Machine
  , Outcome(..)
  , Pending
  , Refusal(..)
  , refusalText
  , outcomeTarget
  , isRefusal
  ) where

import Prelude

import Data.Maybe (Maybe)

-- | Why a transition did not happen.
-- |
-- | A refusal carries a reason because the reason is usually about something
-- | other than the state that refused — "loop 3 holds the converter" — and a
-- | display that can say so is helpful where one that cannot is only
-- | apologetic. Whether this should be a typed reason rather than a string is
-- | still open; see the spec's open questions.
newtype Refusal = Refusal String

derive newtype instance eqRefusal :: Eq Refusal
derive newtype instance ordRefusal :: Ord Refusal

refusalText :: Refusal -> String
refusalText (Refusal reason) = reason

-- | What happened when an event met a state.
-- |
-- | `Refused` is deliberately distinct from `Stay`: doing nothing because the
-- | event is meaningless here is not the same as doing nothing because
-- | something else in the system forbade it, and a press that leaves no trace
-- | is the failure this distinction exists to prevent.
data Outcome state
  = Move state
  | Stay
  | Refused Refusal

-- | Where the machine ends up, given where it was and what happened.
outcomeTarget :: forall state. state -> Outcome state -> state
outcomeTarget current = case _ of
  Move next -> next
  Stay -> current
  Refused _ -> current

isRefusal :: forall state. Outcome state -> Boolean
isRefusal = case _ of
  Refused _ -> true
  _ -> false

-- | A transition that has not happened yet, and when it will.
-- |
-- | Note that this names the **event that will be delivered**, not the state
-- | that will be reached. Naming the target state would let `pending` and
-- | `transition` disagree about what a deadline does, and a diagram drawn from
-- | the first would then lie about the machine driven by the second. Naming the
-- | event keeps `transition` the single authority on where anything goes.
type Pending event deadline =
  { fires :: event
  , at :: deadline
  }

-- | The machine.
-- |
-- | `env` is the shared, changing world — the grid, which loop currently holds
-- | the converter, when the current state was entered. `cfg` is this instance's
-- | own settings. Both are supplied per step rather than baked in at
-- | construction, which is what lets a Mealy built from this value carry state
-- | and nothing else.
-- |
-- | `states` and `events` are what make the machine describable: with both
-- | enumerable, the diagram can be *derived* by running `transition` over their
-- | cross product rather than declared alongside it.
type Machine env cfg state event deadline =
  { initial :: state
  , states :: Array state
  , events :: Array event
  , transition :: env -> cfg -> state -> event -> Outcome state
  , pending :: env -> cfg -> state -> Maybe (Pending event deadline)
  , stateId :: state -> String
  , stateLabel :: state -> String
  , isFinal :: state -> Boolean
  , eventLabel :: event -> String
  }
