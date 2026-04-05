-- | Anscombe's Quartet - Types
-- |
-- | Clean separation of domain types and state machine encoding.
module Anscombe.Types where

import Prelude

import Anscombe.Data (Dataset)
import Data.Maybe (Maybe(..))
import Hylograph.Music.Internal.FFI (AudioContext)

-- =============================================================================
-- Play State Machine
-- =============================================================================

-- | The play state encodes whether audio is playing and which note is active.
-- | This is a proper state machine with clear transitions:
-- |
-- |   Idle ──StartPlaying──► Playing
-- |     ▲                        │
-- |     └────StopPlaying─────────┘
-- |
-- | When Playing, noteIndex cycles through 0..10, then back to 0.
data PlayState
  = Idle
  | Playing PlayingState

derive instance eqPlayState :: Eq PlayState

type PlayingState =
  { dataset :: Dataset      -- Which of the four datasets is playing
  , noteIndex :: Int        -- Current note being highlighted (0-10)
  }

-- =============================================================================
-- Component State
-- =============================================================================

type State =
  { playState :: PlayState
  , audioCtx :: Maybe AudioContext
  }

-- | Initial state: nothing playing, no audio context yet
initialState :: State
initialState =
  { playState: Idle
  , audioCtx: Nothing
  }

-- =============================================================================
-- Predicates
-- =============================================================================

-- | Check if a specific dataset is currently playing
isDatasetPlaying :: String -> PlayState -> Boolean
isDatasetPlaying name = case _ of
  Idle -> false
  Playing ps -> ps.dataset.name == name

-- | Get the current note index for a dataset (Nothing if not playing)
currentNoteFor :: String -> PlayState -> Maybe Int
currentNoteFor name = case _ of
  Idle -> Nothing
  Playing ps
    | ps.dataset.name == name -> Just ps.noteIndex
    | otherwise -> Nothing
