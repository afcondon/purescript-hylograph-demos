-- | Glassbox.Describe
-- |
-- | The other reading of a `Spec`: derive the drawing.
-- |
-- | The diagram is **derived, not declared**. Every edge here is produced by
-- | actually resolving a rule under a world, so there is no second description
-- | that could drift from the first. An earlier sketch gave the machine a
-- | `describe` field alongside its transition function; that would have been two
-- | sources for one truth, and the entire premise of the project is that they
-- | cannot be allowed to diverge.
-- |
-- | Because the drawing is derived **under a world**, config genuinely reshapes
-- | it: set the looper's count-in to zero and Armed loses its only inbound edge;
-- | tell the car radio it has no CD slot and the CD state is stranded. That is
-- | the mechanical test the format uses to sort config from parameter — *does
-- | the diagram change when you change it?* — and it only works because the
-- | picture is computed rather than authored.
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
import Glassbox.Run (World, resolve)
import Glassbox.Spec
  ( EventId
  , Outcome(..)
  , Spec
  , StateId(..)
  , deadlineFor
  , labelOfEvent
  , textOfRefusal
  )
import Data.Graph.InducedTree (EdgeClass, Induced, classOf)

-- | How an edge came to exist.
data EdgeKind
  = OnEvent      -- ^ an event moved the machine
  | OnDeadline   -- ^ it will fire with no event from anyone
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
-- | A second, independent annotation on the same edges. `describe` says how an
-- | edge came to exist; `annotate` says what it means to someone navigating.
annotate :: Induced String -> StateMachine Unit EdgeExtra -> StateMachine Unit EdgeExtra
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

-- | Derive the diagram by resolving every (state, event) pair under one world.
describe :: DescribeOptions -> World -> Spec -> StateMachine Unit EdgeExtra
describe options world spec =
  { states: map toState spec.states
  , transitions: merge (eventEdges <> deadlineEdges)
  }
  where
  toState decl =
    { id: raw decl.id
    , label: decl.label
    , isInitial: decl.id == spec.initial
    , isFinal: decl.final
    , extra: unit
    }

  raw (StateId s) = s

  eventEdges = filter keep (concatMap probeState spec.states)
    where
    keep edge = options.showRefusals || edge.extra.kind /= OnRefusal

  -- An event that only ever arrives because a deadline delivered it is drawn as
  -- a deadline edge below. Probing it here as well would draw the same
  -- transition twice, once truthfully and once misleadingly — as though a
  -- person could cause it.
  probeState decl =
    Array.mapMaybe (probe decl.id)
      (filter (not <<< deliveredByDeadline decl.id) (map _.id spec.events))

  deliveredByDeadline sid event = case deadlineFor spec sid of
    Just d -> d.fires == event
    Nothing -> false

  probe :: StateId -> EventId -> Maybe (Transition EdgeExtra)
  probe from event = case resolve spec world from event of
    Move next -> Just
      { from: raw from
      , to: raw next
      , label: labelOfEvent spec event
      , extra: { kind: OnEvent, reason: Nothing, role: Nothing }
      }
    Refuse rid -> Just
      { from: raw from
      , to: raw from
      , label: labelOfEvent spec event
      , extra: { kind: OnRefusal, reason: Just (textOfRefusal spec rid), role: Nothing }
      }
    Stay -> Nothing

  -- A state that resolves by itself. The target is obtained by asking the rules
  -- what the pending event does, never by trusting the deadline to say where it
  -- goes — which is why a `Deadline` names an event rather than a state.
  deadlineEdges = Array.mapMaybe deadlineEdge spec.states

  deadlineEdge decl = case deadlineFor spec decl.id of
    Nothing -> Nothing
    Just d -> case resolve spec world decl.id d.fires of
      Move next -> Just
        { from: raw decl.id
        , to: raw next
        , label: labelOfEvent spec d.fires
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
