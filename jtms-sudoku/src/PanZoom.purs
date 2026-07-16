-- | Wheel-to-zoom, drag-to-pan on an already-rendered SVG, by direct
-- | viewBox mutation — HATS renders the proof once, exploration is free.
module PanZoom (attachPanZoom) where

import Prelude

import Effect (Effect)

foreign import attachPanZoomImpl :: String -> Effect Unit

attachPanZoom :: String -> Effect Unit
attachPanZoom selector = attachPanZoomImpl selector
