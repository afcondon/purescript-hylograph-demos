module Glassbox.Demo.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Glassbox.Demo.Component as Demo
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  slot <- HA.selectElement (QuerySelector "#glassbox-controls")
  case slot of
    Just element -> void (runUI Demo.component unit element)
    Nothing -> liftEffect (throw "no #glassbox-controls in the page")
