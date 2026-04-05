module Anscombe.Main where

import Prelude

import Anscombe.Component as Anscombe
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  _ <- runUI Anscombe.component unit body
  pure unit
