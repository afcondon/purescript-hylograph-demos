-- | Minimal DOM FFI for the demo pages.
-- | This is the ONLY module with FFI in the demo — all other modules
-- | are pure PureScript.
module Demo.Dom
  ( setSvgContent
  , setCellsSvg
  , setCode
  , elementExists
  , everyTick
  , onClick
  ) where

import Prelude
import Effect (Effect)

-- | Set innerHTML of a DOM element to an SVG string.
foreign import setSvgContent :: String -> String -> Effect Unit

-- | Set multiple SVG cells into a container element.
foreign import setCellsSvg :: String -> Array String -> Effect Unit

-- | Set text content of a code block element.
foreign import setCode :: String -> String -> Effect Unit

-- | Check whether a DOM element exists by id.
foreign import elementExists :: String -> Effect Boolean

-- | Call a handler every `stepMs`, passing the FIXED nominal step as the
-- | delta. Returns a canceller.
-- |
-- | Deliberately a timer rather than `requestAnimationFrame`: rAF does not
-- | fire at all in a hidden or covered tab, so a rAF-driven reveal hangs
-- | silently rather than slowing down. See the note in `Dom.js`.
foreign import everyTick :: Number -> (Number -> Effect Unit) -> Effect (Effect Unit)

-- | Attach a click handler to an element by id. No-op if absent.
foreign import onClick :: String -> Effect Unit -> Effect Unit
