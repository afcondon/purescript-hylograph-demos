-- | Entry point for the hylograph-onion demos.
-- | Detects which page is loaded and dispatches to the right renderer.
module Demo.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Demo.Catalogue as Catalogue
import Demo.Dom as Dom
import Demo.LesMis as LesMis
import Demo.Recipes as Recipes
import Hylograph.HATS (elem) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender) as HATS
import Hylograph.Internal.Element.Types (ElementType(..))
import Onion.Hats (mkBlob, blobTree)
import Onion.Shape (Point)

-- =============================================================================
-- Page dispatch
-- =============================================================================

main :: Effect Unit
main = do
  isCatalogue <- Dom.elementExists "dim-size"
  isLesMis <- Dom.elementExists "lesmis-graph"
  isRecipes <- Dom.elementExists "r1-stage"
  isLanding <- Dom.elementExists "hero"
  if isCatalogue
    then Catalogue.main
    else if isLesMis
      then do
        log "[Onion] Starting Les Mis"
        LesMis.initLesMis
      else if isRecipes
        then Recipes.main
        else if isLanding
          then renderLanding
          else log "[Onion] Unknown page"

-- =============================================================================
-- Landing page — Albers hero (rendered with HATS)
-- =============================================================================

renderLanding :: Effect Unit
renderLanding = do
  log "[Onion] Rendering landing page"

  let wcfg = { layers: 25, depth: 5, displacement: 0.12 }
      triRed    = [ { x: 300.0, y: 30.0 },  { x: 180.0, y: 240.0 }, { x: 420.0, y: 240.0 } ] :: Array Point
      triYellow = [ { x: 180.0, y: 80.0 },  { x: 60.0,  y: 290.0 }, { x: 300.0, y: 290.0 } ] :: Array Point
      triBlue   = [ { x: 420.0, y: 80.0 },  { x: 300.0, y: 290.0 }, { x: 540.0, y: 290.0 } ] :: Array Point
      { blob: bRed }    = mkBlob wcfg 7001 triRed
      { blob: bYellow } = mkBlob wcfg 7002 triYellow
      { blob: bBlue }   = mkBlob wcfg 7003 triBlue

      tree = HATS.elem SVG
        [ F.viewBox 0.0 0.0 600.0 320.0
        , F.preserveAspectRatio "xMidYMid meet"
        ]
        [ blobTree "red" "#d42a2a" bRed
        , blobTree "yellow" "#e8b818" bYellow
        , blobTree "blue" "#2968a8" bBlue
        ]

  _ <- HATS.rerender "#hero" tree
  log "[Onion] Done"
