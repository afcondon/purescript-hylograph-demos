-- | Demo fixtures for `Hylograph.Components.BoxPlot`.
-- |
-- | Exports only data + a `FrameInput` + a container id string. The driver
-- | module (`Demo.Main`) splices the Halogen slot and surrounding HTML.
-- |
-- | Synthetic response-time distributions (ms) for four API endpoints,
-- | ~30 samples each, with a handful of planted outliers per endpoint so
-- | the outlier logic is visible.
module Demo.BoxPlot
  ( ResponseRow
  , apiResponseTimes
  , boxPlotInput
  , containerId
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.Components.BoxPlot as BoxPlot
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Theme (defaultTheme)

type ResponseRow =
  { endpoint :: String
  , ms :: Number
  }

-- | Four endpoints, ~30 samples each, centered around different means with
-- | different spreads, plus 2-4 planted outliers per endpoint.
apiResponseTimes :: Array ResponseRow
apiResponseTimes =
  samplesFor "GET /users"   100  42.0  8.0  [ 180.0, 210.0, 95.0 ]
    <> samplesFor "GET /orders" 200  88.0  18.0 [ 350.0, 420.0, 280.0, 12.0 ]
    <> samplesFor "POST /login" 300  55.0  12.0 [ 230.0, 195.0 ]
    <> samplesFor "GET /search" 400  130.0 30.0 [ 550.0, 620.0, 40.0, 48.0 ]

-- | Generate a synthetic sample distribution: 30 deterministic pseudo-random
-- | values scattered around `mean` with half-width `spread`, plus the
-- | supplied `extras` (planted outliers). `seed` distinguishes distributions.
samplesFor :: String -> Int -> Number -> Number -> Array Number -> Array ResponseRow
samplesFor endpoint seed mean spread extras =
  let
    n = 30
    sampled = Array.range 0 (n - 1) <#> \i ->
      let raw = pseudoNoise (seed + i * 101) -- in [-0.5, 0.5)
      in mean + raw * 2.0 * spread
  in
    (sampled <> extras) <#> \v -> { endpoint, ms: v }

-- | Cheap integer-hash-derived number in [-0.5, 0.5). Deterministic per seed.
pseudoNoise :: Int -> Number
pseudoNoise seed =
  let
    a = (seed * 2147483629) `mod` 10000
    a' = if a < 0 then a + 10000 else a
  in Int.toNumber a' / 10000.0 - 0.5

containerId :: String
containerId = "boxplot-response-times"

boxPlotInput :: FrameInput (BoxPlot.Input Array (endpoint :: String, ms :: Number))
boxPlotInput =
  { containerId
  , chart:
      { config:
          let c0 = BoxPlot.config { category: _.endpoint, value: _.ms }
          in c0
            { layout = c0.layout
                { width = 900.0
                , height = 420.0
                , margin = { top: 20.0, right: 20.0, bottom: 56.0, left: 60.0 }
                }
            , labels = { x: Just "Endpoint", y: Just "Response time (ms)" }
            , highlightGroup = Just "latency"
            }
      , dataset: apiResponseTimes
      , theme: defaultTheme
      }
  }
