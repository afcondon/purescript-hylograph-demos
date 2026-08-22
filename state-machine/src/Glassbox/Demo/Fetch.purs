-- | Glassbox.Demo.Fetch
-- |
-- | Fetching an artifact over HTTP.
-- |
-- | Small on purpose. The point of the exercise is that a machine arrives as
-- | bytes at runtime, so the host needs some way to go and get them; which way
-- | is not interesting, and a whole HTTP client would be a lot of dependency for
-- | one `GET`.
module Glassbox.Demo.Fetch (fetchText) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import fetchTextImpl :: String -> EffectFnAff String

fetchText :: String -> Aff String
fetchText = fromEffectFnAff <<< fetchTextImpl
