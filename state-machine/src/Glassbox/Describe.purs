-- | Glassbox.Describe
-- |
-- | The other reading of a `Machine`: derive the drawing.
-- |
-- | The diagram is **derived, not declared**. Every edge here is produced by
-- | actually calling `transition`, so there is no second description that could
-- | drift from the first. An earlier sketch gave `Machine` a `describe` field
-- | alongside `transition`; that would have been two sources for one truth, and
-- | the entire premise of the project is that they cannot be allowed to diverge.
-- |
-- | Output is `DataViz.Layout.StateMachine`'s own `StateMachine`, so it feeds
-- | that module's `layout` directly. Edge payloads ride in the `Transition`
-- | `extra` field.
module Glassbox.Describe
  ( EdgeKind(..)
  , EdgeExtra
  , DescribeOptions
  , defaultOptions
  , describe
  , annotate
  , machineEdges
  ) where

import Prelude

import Data.Array (concatMap, filter, nub, sortBy)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DataViz.Layout.StateMachine (StateMachine, Transition)
import Glassbox.Machine (Machine, Outcome(..), Pending, refusalText)
import Glassbox.Tree (EdgeClass, Induced, classOf)

-- | How an edge came to exist.
data EdgeKind
  = OnEvent      -- ^ an event moved the machine
  | OnDeadline   -- ^ a pending transition will fire it with no event from anyone
  | OnRefusal    -- ^ the event was refused, with a reason

derive instance eqEdgeKind :: Eq EdgeKind
derive instance ordEdgeKind :: Ord EdgeKind

-- | What rides along on a drawn transition.
-- |
-- | This is what the `extra` parameter added to `DataViz.Layout.StateMachine`'s
-- | `Transition` is for: without it a renderer would need a side table keyed by
-- | from/to/label to know that an edge is a refusal rather than a move.
type EdgeExtra =
  { kind :: EdgeKind
  , reason :: Maybe String
  , role :: Maybe EdgeClass
  }

-- | Attach each edge's position relative to the induced tree.
-- |
-- | A second, independent annotation on the same edges — which is what the
-- | `extra` parameter added to `DataViz.Layout.StateMachine`'s `Transition` is
-- | earning here. `describe` says how an edge came to exist; `annotate` says
-- | what it means to someone navigating.
annotate :: Induced -> StateMachine Unit EdgeExtra -> StateMachine Unit EdgeExtra
annotate induced machine = machine
  { transitions = map mark machine.transitions }
  where
  mark edge = edge { extra = edge.extra { role = classOf induced edge.from edge.to } }

-- | The bare directed edge set, for analysis that does not care about labels.
machineEdges :: forall se te. StateMachine se te -> Array (Tuple String String)
machineEdges machine = map (\e -> Tuple e.from e.to) machine.transitions

type DescribeOptions =
  { showRefusals :: Boolean
  }

defaultOptions :: DescribeOptions
defaultOptions = { showRefusals: true }

-- | Derive the diagram by running `transition` over states x events.
describe
  :: forall env cfg state event deadline
   . Eq state
  => Eq event
  => DescribeOptions
  -> env
  -> cfg
  -> Machine env cfg state event deadline
  -> StateMachine Unit EdgeExtra
describe options env cfg machine =
  { states: map toState machine.states
  , transitions: merge (eventEdges <> deadlineEdges)
  }
  where
  toState state =
    { id: machine.stateId state
    , label: machine.stateLabel state
    , isInitial: state == machine.initial
    , isFinal: machine.isFinal state
    , extra: unit
    }

  -- Every (state, event) pair, asked of the transition function itself.
  eventEdges = filter keep $ concatMap probeState machine.states
    where
    keep edge = options.showRefusals || edge.extra.kind /= OnRefusal

  -- An event that only ever arrives because a deadline delivered it is drawn
  -- as a deadline edge below. Probing it here as well would draw the same
  -- transition twice, once truthfully and once misleadingly — as though a foot
  -- could cause it.
  probeState state =
    Array.mapMaybe (probe state) (filter (not <<< deliveredByDeadline state) machine.events)

  deliveredByDeadline state event = case machine.pending env cfg state of
    Just { fires } -> fires == event
    Nothing -> false

  probe state event =
    case machine.transition env cfg state event of
      Move next -> Just
        { from: machine.stateId state
        , to: machine.stateId next
        , label: machine.eventLabel event
        , extra: { kind: OnEvent, reason: Nothing, role: Nothing }
        }
      Refused refusal -> Just
        { from: machine.stateId state
        , to: machine.stateId state
        , label: machine.eventLabel event
        , extra: { kind: OnRefusal, reason: Just (refusalText refusal), role: Nothing }
        }
      Stay -> Nothing

  -- A state that resolves by itself. The target is obtained by asking
  -- `transition` what the pending event does, never by trusting `pending` to
  -- say where it goes.
  deadlineEdges = Array.mapMaybe deadlineEdge machine.states

  deadlineEdge state = machine.pending env cfg state >>= edgeFor state

  edgeFor :: state -> Pending event deadline -> Maybe (Transition EdgeExtra)
  edgeFor state { fires } =
    case machine.transition env cfg state fires of
      Move next -> Just
        { from: machine.stateId state
        , to: machine.stateId next
        , label: machine.eventLabel fires
        , extra: { kind: OnDeadline, reason: Nothing, role: Nothing }
        }
      _ -> Nothing

-- | Collapse parallel edges of the same kind between the same pair, joining
-- | their labels, so three events that all lead one way draw one arrow.
merge :: Array (Transition EdgeExtra) -> Array (Transition EdgeExtra)
merge edges = map rebuild (nub (map key edges))
  where
  key edge = Tuple (Tuple edge.from edge.to) edge.extra.kind

  rebuild k@(Tuple (Tuple from to) kind) =
    let
      group = filter (\e -> key e == k) edges
      labels = nub (map _.label group)
      reasons = Array.mapMaybe (\e -> e.extra.reason) group
    in
      { from
      , to
      , label: joinWith ", " (sortBy compare labels)
      , extra:
          { kind
          , reason: Array.head reasons
          , role: Nothing
          }
      }
