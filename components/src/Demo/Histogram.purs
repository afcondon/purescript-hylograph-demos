-- | Demo fixtures for the histogram pattern.
-- |
-- | There is no `Histogram` module — a histogram here is literally a
-- | `BarChart` fed pre-binned rows, with `barPadding` set to 0 so bars
-- | touch. Two demos:
-- |
-- | - `equalWidthInput` — 12 equal-width bins over a synthetic bimodal
-- |   distribution. Classic histogram look.
-- | - `equalCountInput` — 12 quantile bins over the same data. Each bar
-- |   ends up roughly the same height; the bin-range *labels* carry the
-- |   distributional shape (narrow ranges where samples cluster, wide
-- |   where they don't). Limitation: BarChart's x-axis is categorical, so
-- |   bar *widths* are uniform on screen; label text is the fallback.
module Demo.Histogram
  ( equalWidthInput
  , equalCountInput
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.Components.BarChart as BarChart
import Hylograph.Components.Binning as Binning
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Theme (defaultTheme)

--------------------------------------------------------------------------------
-- Synthetic dataset — bimodal: main cluster near 50, secondary near 85
--------------------------------------------------------------------------------

measurements :: Array Number
measurements = Array.range 0 299 <#> \i ->
  let
    r1 = pseudoNoise i
    r2 = pseudoNoise (i + 31337)
    r3 = pseudoNoise (i + 12345)
    noise = r1 + r2 + r3  -- roughly triangular in [-1.5, 1.5]
  in
    if i < 200
      then 50.0 + 12.0 * noise
      else 85.0 + 6.0 * noise

-- | Deterministic integer-hash-derived number in [-0.5, 0.5). Same
-- | recipe as `Demo.Heatmap.pseudoNoise` — keeping it local so this file
-- | stays self-contained.
pseudoNoise :: Int -> Number
pseudoNoise seed =
  let
    a = (seed * 2147483629) `mod` 10000
    a' = if a < 0 then a + 10000 else a
  in
    Int.toNumber a' / 10000.0 - 0.5

--------------------------------------------------------------------------------
-- Bin row shape (shared by both demos)
--------------------------------------------------------------------------------

type BinRow =
  { label :: String
  , binStart :: Number
  , binEnd :: Number
  , count :: Int
  }

toBinRows :: Array Binning.Bin -> Array BinRow
toBinRows = map \b ->
  { label: Binning.defaultLabel b
  , binStart: b.binStart
  , binEnd: b.binEnd
  , count: b.count
  }

--------------------------------------------------------------------------------
-- Equal-width bins
--------------------------------------------------------------------------------

equalWidthBins :: Array BinRow
equalWidthBins = toBinRows (Binning.equalWidth 12 measurements)

equalWidthInput
  :: FrameInput
       (BarChart.Input Array
         (label :: String, binStart :: Number, binEnd :: Number, count :: Int))
equalWidthInput =
  { containerId: "histogram-equal-width"
  , chart:
      { config:
          let c0 = BarChart.config
                     { category: _.label
                     , value: Int.toNumber <<< _.count
                     }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Measurement range", y: Just "Count" }
            , barPadding = 0.0
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r ->
                    r.label <> "  ·  " <> show r.count <> " samples"
                }
            }
      , dataset: equalWidthBins
      , theme: defaultTheme
      }
  }

--------------------------------------------------------------------------------
-- Equal-count (quantile) bins
--------------------------------------------------------------------------------

equalCountBins :: Array BinRow
equalCountBins = toBinRows (Binning.equalCount 12 measurements)

equalCountInput
  :: FrameInput
       (BarChart.Input Array
         (label :: String, binStart :: Number, binEnd :: Number, count :: Int))
equalCountInput =
  { containerId: "histogram-equal-count"
  , chart:
      { config:
          let c0 = BarChart.config
                     { category: _.label
                     , value: Int.toNumber <<< _.count
                     }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Quantile range", y: Just "Count" }
            , barPadding = 0.0
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r ->
                    r.label <> "  ·  " <> show r.count <> " samples  ·  width "
                      <> show (r.binEnd - r.binStart)
                }
            }
      , dataset: equalCountBins
      , theme: defaultTheme
      }
  }
