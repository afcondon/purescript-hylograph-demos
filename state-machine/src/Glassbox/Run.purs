-- | Glassbox.Run
-- |
-- | Running a decoded `Spec`.
-- |
-- | The interpreter is a **pure function of (world, state, event)** and nothing
-- | else. It holds no state of its own, so a host stores the current `StateId` —
-- | a plain, comparable, serialisable value — rather than an opaque machine
-- | object. The previous design handed the host a `Mealy`, which meant the
-- | current state lived inside a closure and the same phase was stored twice:
-- | once where it could be read and once where it could not. That cost session
-- | restore, time-travel, and the ability to paste a reproducing trace into a
-- | bug report, and bought nothing.
-- |
-- | `toMealy` remains for anyone composing with the stream-transducer
-- | ecosystem; it is `unfoldMealy` over `step` and carries no extra meaning.
module Glassbox.Run
  ( World
  , worldFrom
  , setConfig
  , setFact
  , lookupConfig
  , lookupFact
  , evalExpr
  , evalGuard
  , resolve
  , step
  , Step
  , initialStep
  , dueAt
  ) where

import Prelude

import Data.Array (find, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Glassbox.Spec
  ( ConfigId
  , EventId
  , Expr(..)
  , FactId
  , Guard(..)
  , CmpOp(..)
  , Outcome(..)
  , Spec
  , StateId
  , Value(..)
  , deadlineFor
  , outcomeTarget
  , ruleFor
  , truthy
  )

-- | The world a guard may read: what the artifact declares, and what the host
-- | reports.
-- |
-- | Both arrive per step rather than being captured once, because both genuinely
-- | change between events — a setting is turned, another loop takes the
-- | converter — and because a captured world is a closure by another name.
type World =
  { config :: Map ConfigId Value
  , facts :: Map FactId Value
  }

-- | The world implied by a spec alone: every config at its declared default and
-- | no facts. What a host starts from before it knows anything.
worldFrom :: Spec -> World
worldFrom spec =
  { config: foldl (\m c -> Map.insert c.id c.default m) Map.empty spec.config
  , facts: Map.empty
  }

setConfig :: ConfigId -> Value -> World -> World
setConfig k v world = world { config = Map.insert k v world.config }

setFact :: FactId -> Value -> World -> World
setFact k v world = world { facts = Map.insert k v world.facts }

lookupConfig :: World -> ConfigId -> Maybe Value
lookupConfig world k = Map.lookup k world.config

lookupFact :: World -> FactId -> Maybe Value
lookupFact world k = Map.lookup k world.facts

-- =============================================================================
-- Evaluation
-- =============================================================================

-- | An expression that names something the world does not have evaluates to
-- | `Nothing` rather than to a default. A missing fact is not a false fact, and
-- | quietly reading it as one is how a machine takes a branch nobody authored.
-- | The vocabulary lint is what stops such an artifact loading at all.
evalExpr :: World -> Expr -> Maybe Value
evalExpr world = case _ of
  Lit v -> Just v
  ConfigOf k -> lookupConfig world k
  FactOf k -> lookupFact world k

evalGuard :: World -> Guard -> Boolean
evalGuard world = case _ of
  Holds e -> maybe false truthy (evalExpr world e)
  Not g -> not (evalGuard world g)
  And gs -> foldl (\acc g -> acc && evalGuard world g) true gs
  Or gs -> foldl (\acc g -> acc || evalGuard world g) false gs
  Cmp op l r -> case evalExpr world l, evalExpr world r of
    Just a, Just b -> compareValues op a b
    _, _ -> false

-- | Equality is structural; ordering requires both sides to be the same kind of
-- | value. Ordering a number against a string is a mistake in the artifact, and
-- | it reads as false rather than falling back on constructor order, which would
-- | be a well-defined answer to a question nobody asked.
compareValues :: CmpOp -> Value -> Value -> Boolean
compareValues op a b = case op of
  OpEq -> a == b
  OpNe -> a /= b
  _ -> case ordering of
    Nothing -> false
    Just ord -> case op of
      OpLt -> ord == LT
      OpLe -> ord /= GT
      OpGt -> ord == GT
      OpGe -> ord /= LT
      _ -> false
  where
  ordering = case a, b of
    VNumber x, VNumber y -> Just (compare x y)
    VString x, VString y -> Just (compare x y)
    VBoolean x, VBoolean y -> Just (compare x y)
    _, _ -> Nothing

-- =============================================================================
-- Stepping
-- =============================================================================

-- | What a state does with an event, under a given world.
-- |
-- | Cases are tried in order and the first whose guard holds wins; a case with
-- | no guard is the catch-all. A state and event with no rule at all yields
-- | `Stay`, which is the same answer a flat machine gives for "not mine" — the
-- | totality lint is what ensures a real artifact never relies on it.
resolve :: Spec -> World -> StateId -> EventId -> Outcome
resolve spec world from on = case ruleFor spec from on of
  Nothing -> Stay
  Just rule -> case find matches rule.cases of
    Nothing -> Stay
    Just c -> c.outcome
  where
  matches c = case c.when of
    Nothing -> true
    Just g -> evalGuard world g

-- | Where it was, what happened, where it now is.
-- |
-- | `outcome` is carried out rather than collapsed so that a refusal reaches the
-- | display instead of vanishing into an unchanged state.
type Step =
  { from :: StateId
  , event :: EventId
  , outcome :: Outcome
  , current :: StateId
  }

-- | One step. Pure, total, and the only thing a host needs to run a machine.
step :: Spec -> World -> StateId -> EventId -> Tuple StateId Step
step spec world from event =
  let
    outcome = resolve spec world from event
    current = outcomeTarget from outcome
  in
    Tuple current { from, event, outcome, current }

-- | The step a machine is notionally in before anything has been fed to it.
initialStep :: Spec -> EventId -> Step
initialStep spec event =
  { from: spec.initial
  , event
  , outcome: Stay
  , current: spec.initial
  }

-- | When the deadline for a state falls due, relative to when it was entered.
-- |
-- | `Nothing` when the state resolves only on events, or when the deadline's
-- | quantity does not evaluate to a number under this world.
dueAt :: Spec -> World -> StateId -> Number -> Maybe { fires :: EventId, at :: Number }
dueAt spec world sid enteredAt = case deadlineFor spec sid of
  Nothing -> Nothing
  Just d -> case evalExpr world d.after of
    Just (VNumber n) -> Just { fires: d.fires, at: enteredAt + n }
    _ -> Nothing
