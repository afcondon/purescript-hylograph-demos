-- | Anscombe's Quartet - Timer
-- |
-- | Wrapper around Effect.Timer for Halogen subscriptions.
-- | Accepts Number for milliseconds (converts internally to Int).
module Anscombe.Timer
  ( IntervalId
  , setInterval
  , clearInterval
  , setTimeout
  ) where

import Prelude

import Data.Int (round)
import Effect (Effect)
import Effect.Timer as T

-- | Opaque handle for cancelling an interval
type IntervalId = T.IntervalId

-- | Set up a repeating callback. Returns handle for cancellation.
-- | Milliseconds is rounded to nearest integer.
setInterval :: Number -> Effect Unit -> Effect IntervalId
setInterval ms action = T.setInterval (round ms) action

-- | Cancel a repeating interval
clearInterval :: IntervalId -> Effect Unit
clearInterval = T.clearInterval

-- | Set up a one-shot callback after a delay (milliseconds)
-- | Milliseconds is rounded to nearest integer.
setTimeout :: Number -> Effect Unit -> Effect Unit
setTimeout ms action = void $ T.setTimeout (round ms) action
