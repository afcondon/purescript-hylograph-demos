-- | The page's clock for the film, and a reload for the replay button.
module Macondo.Clock
  ( filmMode
  , exposeStep
  , reload
  , setOpacity
  ) where

import Prelude

import Effect (Effect)

-- | Whether the page was opened as a film (`?film`): stepped from outside, a
-- | frame at a time, instead of by requestAnimationFrame.
foreign import filmMode :: Effect Boolean

-- | Let the renderer step the page: `window.macondo.step(n)` runs n frames,
-- | and `window.macondo.cues` holds the frames the soundtrack is laid against.
foreign import exposeStep :: forall cues. (Int -> Effect Unit) -> cues -> Effect Unit

foreign import reload :: Effect Unit

-- | Set the opacity of every element the selector finds.
foreign import setOpacity :: String -> Number -> Effect Unit
