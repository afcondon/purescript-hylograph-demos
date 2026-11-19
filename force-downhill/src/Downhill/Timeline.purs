-- | The timeline: a run of the simulation as a row of moments that can be
-- | gone back to. Pure; the simulation itself is a `Hylograph.Simulation.Stepped`.
module Downhill.Timeline
  ( Moment
  , Timeline
  , begin
  , record
  , goTo
  , now
  , cutAfterNow
  , replaceNow
  , momentAt
  , every
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Downhill.Physics (BodyFields, Point)
import Hylograph.Simulation.Stepped (Snapshot)

-- | The simulation at one tick: enough to draw it (`places`, `pinned`) and
-- | everything needed to go on from it (`snapshot`).
type Moment =
  { tick :: Int
  , snapshot :: Snapshot BodyFields
  , places :: Array Point
  , pinned :: Array Int -- the places in the node array of the pinned nodes
  }

-- | The moments recorded so far, earliest first, and which one is shown.
type Timeline =
  { moments :: Array Moment
  , shown :: Int
  }

-- | A moment is recorded every this many ticks.
every :: Int
every = 5

begin :: Moment -> Timeline
begin first = { moments: [ first ], shown: 0 }

-- | Add a moment at the end, and show it.
record :: Moment -> Timeline -> Timeline
record moment timeline =
  { moments: Array.snoc timeline.moments moment, shown: Array.length timeline.moments }

-- | Show another moment (the nearest there is, if there is none there).
goTo :: Int -> Timeline -> Timeline
goTo place timeline = timeline { shown = clamp 0 (Array.length timeline.moments - 1) place }

now :: Timeline -> Maybe Moment
now timeline = Array.index timeline.moments timeline.shown

-- | Forget what came after the moment shown: it is about to be lived again.
cutAfterNow :: Timeline -> Timeline
cutAfterNow timeline = timeline { moments = Array.take (timeline.shown + 1) timeline.moments }

-- | Put another moment in the place of the one shown, and forget what came
-- | after: the moment has been changed, so what followed no longer follows.
replaceNow :: Moment -> Timeline -> Timeline
replaceNow moment timeline =
  cutAfterNow (timeline { moments = fromMaybe timeline.moments (Array.updateAt timeline.shown moment timeline.moments) })

-- | The last moment at or before a tick (the first, if there is none).
momentAt :: Int -> Timeline -> Maybe Moment
momentAt tick timeline = case Array.last (Array.takeWhile (\moment -> moment.tick <= tick) timeline.moments) of
  Just moment -> Just moment
  Nothing -> Array.head timeline.moments
