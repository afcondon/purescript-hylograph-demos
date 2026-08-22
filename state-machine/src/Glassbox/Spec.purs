-- | Glassbox.Spec
-- |
-- | A state machine as **data loaded at runtime**, not as code.
-- |
-- | This is the shape a decoded artifact takes. Nothing here is a function, so
-- | every part of a machine can be serialised, hashed, diffed, drawn and linted;
-- | the previous design made `transition` a PureScript function, which meant a
-- | machine could only be written by a programmer and could never leave the
-- | binary. See `docs/kb/plans/the-same-machine.md`.
-- |
-- | ### The three vocabularies
-- |
-- | The format has three vocabularies and they are owned by different people.
-- |
-- |   * **States and events** are opaque identifiers owned by the machine's
-- |     author. No runtime interprets them; it moves between them and reports
-- |     where it is. This is why they are `String` newtypes rather than a sum
-- |     type — a sum type would mean the set was fixed by whoever compiled the
-- |     host, which is exactly the property being given up.
-- |   * **Config and facts** are the two halves of the world a guard may read.
-- |     Config is declared by the artifact and reshapes the machine (a count-in
-- |     of zero genuinely removes a state); facts are supplied by the host at
-- |     each step and describe the world (another loop holds the converter).
-- |   * **Refusals** are a closed list the artifact declares, so a refusal
-- |     carries a tag rather than prose. A second runtime cannot pattern-match
-- |     English, and a display that wants to draw a refusal needs to know which
-- |     one it was.
-- |
-- | ### What is deliberately absent
-- |
-- | **Effects.** A transition can move, stay or refuse; it cannot yet ask the
-- | host to *do* anything. Whether commands hang off edges or off state entry
-- | and exit is an open design fork, and the format version field exists so that
-- | it can be answered later without breaking artifacts written today.
module Glassbox.Spec
  ( StateId(..)
  , EventId(..)
  , RefusalId(..)
  , ConfigId(..)
  , FactId(..)
  , Value(..)
  , valueToString
  , truthy
  , Expr(..)
  , CmpOp(..)
  , Guard(..)
  , Outcome(..)
  , outcomeTarget
  , isRefusal
  , Case
  , Rule
  , Deadline
  , StateDecl
  , EventDecl
  , EventSource(..)
  , ConfigDecl
  , FactDecl
  , RefusalDecl
  , Spec
  , formatVersion
  , stateIds
  , eventIds
  , labelOfState
  , labelOfEvent
  , textOfRefusal
  , ruleFor
  , deadlineFor
  , userEvents
  ) where

import Prelude

import Data.Array (filter, find, head)
import Data.Maybe (Maybe, maybe)
import Data.Number.Format (toString)

-- | The wire format version this module reads and writes.
-- |
-- | Present from the first commit because a second implementation — in another
-- | language, on another runtime — will read these artifacts, and a format with
-- | two implementations and no version is a format that cannot be changed.
formatVersion :: Int
formatVersion = 1

-- =============================================================================
-- Identifiers
-- =============================================================================

newtype StateId = StateId String

derive newtype instance eqStateId :: Eq StateId
derive newtype instance ordStateId :: Ord StateId
derive newtype instance showStateId :: Show StateId

newtype EventId = EventId String

derive newtype instance eqEventId :: Eq EventId
derive newtype instance ordEventId :: Ord EventId
derive newtype instance showEventId :: Show EventId

newtype RefusalId = RefusalId String

derive newtype instance eqRefusalId :: Eq RefusalId
derive newtype instance ordRefusalId :: Ord RefusalId
derive newtype instance showRefusalId :: Show RefusalId

newtype ConfigId = ConfigId String

derive newtype instance eqConfigId :: Eq ConfigId
derive newtype instance ordConfigId :: Ord ConfigId
derive newtype instance showConfigId :: Show ConfigId

newtype FactId = FactId String

derive newtype instance eqFactId :: Eq FactId
derive newtype instance ordFactId :: Ord FactId
derive newtype instance showFactId :: Show FactId

-- =============================================================================
-- Values, expressions and guards
-- =============================================================================

-- | What a config setting or a fact can hold.
data Value
  = VNumber Number
  | VBoolean Boolean
  | VString String

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value

instance showValue :: Show Value where
  show = valueToString

valueToString :: Value -> String
valueToString = case _ of
  VNumber n -> toString n
  VBoolean b -> if b then "true" else "false"
  VString s -> s

-- | Whether a value counts as true where a guard expects a condition.
-- |
-- | Only a boolean is true or false. A number or a string used where a
-- | condition belongs is a mistake in the artifact rather than a value to
-- | coerce, and it reads as false so that the vocabulary lint has something to
-- | catch rather than the machine silently taking a branch.
truthy :: Value -> Boolean
truthy = case _ of
  VBoolean b -> b
  _ -> false

-- | Something a guard can evaluate to a value.
data Expr
  = Lit Value
  | ConfigOf ConfigId
  | FactOf FactId

derive instance eqExpr :: Eq Expr

data CmpOp = OpEq | OpNe | OpLt | OpLe | OpGt | OpGe

derive instance eqCmpOp :: Eq CmpOp
derive instance ordCmpOp :: Ord CmpOp

-- | A condition on the world, as data.
data Guard
  = Holds Expr
  | Not Guard
  | And (Array Guard)
  | Or (Array Guard)
  | Cmp CmpOp Expr Expr

derive instance eqGuard :: Eq Guard

-- =============================================================================
-- Outcomes and rules
-- =============================================================================

-- | What happened when an event met a state.
-- |
-- | `Stay` is deliberately distinct from `Refuse`: doing nothing because the
-- | event is meaningless here is not the same as doing nothing because something
-- | else in the system forbade it, and a press that leaves no trace is the
-- | failure this distinction exists to prevent.
data Outcome
  = Move StateId
  | Stay
  | Refuse RefusalId

derive instance eqOutcome :: Eq Outcome

-- | Where the machine ends up, given where it was and what happened.
outcomeTarget :: StateId -> Outcome -> StateId
outcomeTarget current = case _ of
  Move next -> next
  Stay -> current
  Refuse _ -> current

isRefusal :: Outcome -> Boolean
isRefusal = case _ of
  Refuse _ -> true
  _ -> false

-- | One branch of a rule.
-- |
-- | A `when` of `Nothing` is the catch-all. Cases are tried in order and the
-- | first match wins, which makes the machine deterministic by construction
-- | rather than by a check — at the cost that an artifact can hide an
-- | unreachable case behind an earlier one. That is what the overlap lint is
-- | for; it is not a soundness problem.
type Case =
  { when :: Maybe Guard
  , outcome :: Outcome
  }

-- | What one state does with one event.
type Rule =
  { from :: StateId
  , on :: EventId
  , cases :: Array Case
  }

-- | A transition that fires with no event from anybody.
-- |
-- | Names the **event that will be delivered**, not the state that will be
-- | reached, so that `rules` stays the single authority on where anything goes.
-- | Naming a target here would let the deadline and the rules disagree, and a
-- | diagram drawn from one would then lie about the machine driven by the other.
-- |
-- | `after` is a bare quantity in the artifact's own unit. What a `Deadline`
-- | should be so that it admits frames and beats and milliseconds without the
-- | format importing any of them is an open question in the plan.
type Deadline =
  { inState :: StateId
  , fires :: EventId
  , after :: Expr
  }

-- =============================================================================
-- Declarations
-- =============================================================================

type StateDecl =
  { id :: StateId
  , label :: String
  , final :: Boolean
  }

-- | Who delivers an event.
-- |
-- | A machine that draws a button for every event would offer the reader a
-- | button for the count-in elapsing, which misrepresents the machine. The
-- | distinction is in the artifact rather than inferred, because only the author
-- | knows it.
data EventSource
  = FromUser
  | FromRuntime

derive instance eqEventSource :: Eq EventSource
derive instance ordEventSource :: Ord EventSource

type EventDecl =
  { id :: EventId
  , label :: String
  , source :: EventSource
  }

type ConfigDecl =
  { id :: ConfigId
  , label :: String
  , default :: Value
  }

type FactDecl =
  { id :: FactId
  , label :: String
  }

type RefusalDecl =
  { id :: RefusalId
  , text :: String
  }

-- | A whole machine, as decoded from an artifact.
type Spec =
  { version :: Int
  , id :: String
  , title :: String
  , initial :: StateId
  , states :: Array StateDecl
  , events :: Array EventDecl
  , config :: Array ConfigDecl
  , facts :: Array FactDecl
  , refusals :: Array RefusalDecl
  , rules :: Array Rule
  , deadlines :: Array Deadline
  }

-- =============================================================================
-- Lookups
-- =============================================================================

stateIds :: Spec -> Array StateId
stateIds spec = map _.id spec.states

eventIds :: Spec -> Array EventId
eventIds spec = map _.id spec.events

-- | Events a person can cause, which is what a surface should offer buttons for.
userEvents :: Spec -> Array EventId
userEvents spec = map _.id (filter (\e -> e.source == FromUser) spec.events)

labelOfState :: Spec -> StateId -> String
labelOfState spec sid@(StateId raw) =
  maybe raw _.label (find (\s -> s.id == sid) spec.states)

labelOfEvent :: Spec -> EventId -> String
labelOfEvent spec eid@(EventId raw) =
  maybe raw _.label (find (\e -> e.id == eid) spec.events)

textOfRefusal :: Spec -> RefusalId -> String
textOfRefusal spec rid@(RefusalId raw) =
  maybe raw _.text (find (\r -> r.id == rid) spec.refusals)

ruleFor :: Spec -> StateId -> EventId -> Maybe Rule
ruleFor spec from on = find (\r -> r.from == from && r.on == on) spec.rules

deadlineFor :: Spec -> StateId -> Maybe Deadline
deadlineFor spec sid = head (filter (\d -> d.inState == sid) spec.deadlines)
