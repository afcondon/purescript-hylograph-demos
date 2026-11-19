-- | The page's one animation loop, as a Halogen subscription: it emits once
-- | per animation frame for as long as it is subscribed to, and stops when
-- | the subscription ends.
module Honeycomb.AnimationFrames (animationFrames) where

import Prelude

import Effect.Ref as Ref
import Halogen.Subscription (Emitter, makeEmitter)
import Hylograph.Transition.RAF (Timestamp, requestAnimationFrame)

animationFrames :: Emitter Timestamp
animationFrames = makeEmitter \emit -> do
  subscribed <- Ref.new true
  let
    loop timestamp = do
      running <- Ref.read subscribed
      when running do
        emit timestamp
        void $ requestAnimationFrame loop
  void $ requestAnimationFrame loop
  pure (Ref.write false subscribed)
