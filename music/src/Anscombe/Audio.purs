-- | Anscombe's Quartet - Audio
-- |
-- | Sonification using psd3-music's Web Audio API.
-- | Y-coordinate maps directly to pitch, creating an audible "shape" of each dataset.
module Anscombe.Audio
  ( scheduleDatasetCycle
  , cycleParams
  ) where

import Prelude

import Anscombe.Data (Dataset, Point)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Music.Internal.FFI (AudioContext, scheduleNote)

-- =============================================================================
-- Configuration
-- =============================================================================

-- | Timing parameters for a playback cycle
type CycleParams =
  { noteInterval :: Number    -- Seconds between notes
  , noteDuration :: Number    -- How long each note sounds
  , cyclePause :: Number      -- Pause after last note before next cycle
  , volume :: Number          -- 0.0 to 1.0
  }

-- | Default cycle parameters - tuned for pleasant sonification
cycleParams :: CycleParams
cycleParams =
  { noteInterval: 0.175       -- 175ms between notes
  , noteDuration: 0.15        -- Each note lasts 150ms
  , cyclePause: 0.400         -- 400ms pause between cycles
  , volume: 0.4               -- Moderate volume
  }

-- | Total duration of one cycle in milliseconds
cycleDuration :: CycleParams -> Int -> Number
cycleDuration params noteCount =
  (toNumber noteCount * params.noteInterval + params.cyclePause) * 1000.0

-- =============================================================================
-- Pitch Mapping
-- =============================================================================

-- | Map Y-coordinate to frequency (Hz)
-- | Range: Y ∈ [0, 14] → frequency ∈ [200, 900] Hz
-- | This creates an audible melodic contour that traces the dataset's shape.
yToPitch :: Number -> Number
yToPitch y = 200.0 + (y * 50.0)

-- =============================================================================
-- Scheduling
-- =============================================================================

-- | Schedule one cycle of notes for a dataset.
-- | Notes are scheduled ahead of time using Web Audio's precise timing.
-- | Returns the cycle duration in milliseconds.
scheduleDatasetCycle :: AudioContext -> Dataset -> Effect Number
scheduleDatasetCycle ctx dataset = do
  let params = cycleParams

  traverse_ (schedulePoint ctx params) $
    Array.mapWithIndex Tuple dataset.points

  pure $ cycleDuration params (Array.length dataset.points)

-- | Schedule a single point as a note
schedulePoint :: AudioContext -> CycleParams -> Tuple Int Point -> Effect Unit
schedulePoint ctx params (Tuple idx point) =
  scheduleNote ctx
    { time: toNumber idx * params.noteInterval
    , frequency: yToPitch point.y
    , duration: params.noteDuration
    , volume: params.volume
    , waveform: "sine"
    }
