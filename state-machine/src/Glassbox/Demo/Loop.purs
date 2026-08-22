-- | Glassbox.Demo.Loop
-- |
-- | One loop of the six-loop looper, as a describable machine.
-- |
-- | Small enough to draw legibly and real enough not to be a toy. It exercises
-- | the three things a general FSM library does not give us:
-- |
-- |   * **configuration reshapes the machine** — with a count-in configured,
-- |     Record goes to Armed; without one it goes straight to Recording, and
-- |     the *diagram changes* when the setting does. This is recovered from
-- |     `git show ce3ab96^:src/Data/Loopy.purs`, where `transition` was
-- |     parameterised by `ClipSettings` for exactly this reason.
-- |   * **a state that resolves by itself** — Armed becomes Recording when the
-- |     count-in elapses, with no event from anybody.
-- |   * **refusal with a reason** — one audio converter, so a second loop
-- |     cannot arm while another holds it.
module Glassbox.Demo.Loop
  ( Phase(..)
  , LoopEvent(..)
  , Env
  , Cfg
  , Beats
  , loopMachine
  , userEvents
  , defaultEnv
  , defaultCfg
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Glassbox.Machine (Machine, Outcome(..), Refusal(..))

type Beats = Number

data Phase
  = Empty
  | Armed
  | Recording
  | Playing
  | Overdubbing
  | Stopped

derive instance eqPhase :: Eq Phase
derive instance ordPhase :: Ord Phase

data LoopEvent
  = PressRecord
  | PressPlay
  | PressStop
  | PressClear
  | Elapsed   -- ^ delivered by the runtime when a deadline arrives, not by a foot

derive instance eqLoopEvent :: Eq LoopEvent
derive instance ordLoopEvent :: Ord LoopEvent

-- | The shared, changing world.
-- |
-- | `enteredAt` is here rather than in the state because a deadline is a
-- | function of *when the current state was entered*, and folding that into the
-- | state would make Armed-at-beat-4 and Armed-at-beat-8 different states,
-- | destroying the enumerability the drawing depends on.
type Env =
  { nowBeats :: Beats
  , enteredAt :: Beats
  , converterHeldBy :: Maybe Int
  }

-- | This loop's own settings.
type Cfg =
  { countInBeats :: Beats
  , loopIndex :: Int
  }

defaultEnv :: Env
defaultEnv = { nowBeats: 0.0, enteredAt: 0.0, converterHeldBy: Nothing }

defaultCfg :: Cfg
defaultCfg = { countInBeats: 4.0, loopIndex: 1 }

-- | The events a foot can cause. `Elapsed` is excluded: it is delivered by the
-- | clock, and giving it a button would misrepresent the machine.
userEvents :: Array LoopEvent
userEvents = [ PressRecord, PressPlay, PressStop, PressClear ]

loopMachine :: Machine Env Cfg Phase LoopEvent Beats
loopMachine =
  { initial: Empty
  , states: [ Empty, Armed, Recording, Playing, Overdubbing, Stopped ]
  , events: [ PressRecord, PressPlay, PressStop, PressClear, Elapsed ]
  , transition
  , pending
  , stateId
  , stateLabel
  , isFinal: const false
  , eventLabel
  }

transition :: Env -> Cfg -> Phase -> LoopEvent -> Outcome Phase
transition env cfg phase event = case phase, event of
  Empty, PressRecord -> case env.converterHeldBy of
    Just holder | holder /= cfg.loopIndex ->
      Refused (Refusal ("loop " <> show holder <> " holds the converter"))
    _ ->
      -- Configuration reshaping the machine, not merely parameterising it.
      Move (if cfg.countInBeats > 0.0 then Armed else Recording)
  Empty, PressPlay -> Refused (Refusal "nothing recorded yet")
  Empty, PressClear -> Stay
  Empty, PressStop -> Stay
  Empty, Elapsed -> Stay

  Armed, Elapsed -> Move Recording
  Armed, PressRecord -> Move Empty
  Armed, PressStop -> Move Empty
  Armed, PressPlay -> Stay
  Armed, PressClear -> Move Empty

  Recording, PressRecord -> Move Playing
  Recording, PressPlay -> Move Playing
  Recording, PressStop -> Move Stopped
  Recording, PressClear -> Move Empty
  Recording, Elapsed -> Stay

  Playing, PressRecord -> Move Overdubbing
  Playing, PressStop -> Move Stopped
  Playing, PressClear -> Move Empty
  Playing, PressPlay -> Stay
  Playing, Elapsed -> Stay

  Overdubbing, PressRecord -> Move Playing
  Overdubbing, PressPlay -> Move Playing
  Overdubbing, PressStop -> Move Stopped
  Overdubbing, PressClear -> Move Empty
  Overdubbing, Elapsed -> Stay

  Stopped, PressPlay -> Move Playing
  Stopped, PressClear -> Move Empty
  Stopped, PressRecord -> Refused (Refusal "stopped loops overdub from Playing")
  Stopped, PressStop -> Stay
  Stopped, Elapsed -> Stay

-- | Which states resolve by themselves, and when.
-- |
-- | Names the event, not the destination, so `transition` above stays the only
-- | authority on where a deadline leads.
pending :: Env -> Cfg -> Phase -> Maybe { fires :: LoopEvent, at :: Beats }
pending env cfg = case _ of
  Armed -> Just { fires: Elapsed, at: env.enteredAt + cfg.countInBeats }
  _ -> Nothing

stateId :: Phase -> String
stateId = case _ of
  Empty -> "empty"
  Armed -> "armed"
  Recording -> "recording"
  Playing -> "playing"
  Overdubbing -> "overdubbing"
  Stopped -> "stopped"

stateLabel :: Phase -> String
stateLabel = case _ of
  Empty -> "Empty"
  Armed -> "Armed"
  Recording -> "Recording"
  Playing -> "Playing"
  Overdubbing -> "Overdub"
  Stopped -> "Stopped"

eventLabel :: LoopEvent -> String
eventLabel = case _ of
  PressRecord -> "record"
  PressPlay -> "play"
  PressStop -> "stop"
  PressClear -> "clear"
  Elapsed -> "count-in"
