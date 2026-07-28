-- | Demo fixtures for `Hylograph.Components.Legend`.
-- |
-- | Three inputs:
-- | - `lineLegendInput`   — solid-line swatches for the multi-series LinePlot.
-- | - `scatterLegendInput` — circle swatches matching the scatter-by-year.
-- | - `styleShowcaseInput` — one of every swatch shape + LineStyle.
-- |
-- | Series names are hard-coded here rather than imported from `Demo.Main`
-- | to avoid a cyclic import; they match the datasets used there.
module Demo.Legend
  ( lineLegendInput
  , scatterLegendInput
  , styleShowcaseInput
  ) where

import Prelude

import Data.Array as Array
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Legend as Legend
import Hylograph.Components.Mark as Mark
import Hylograph.Components.Series (seriesColor)
import Hylograph.Components.Theme (defaultTheme)

-- Must match `financialsTimeSeries` in Demo.Main.
financialsSeries :: Array String
financialsSeries = [ "Revenue", "Costs" ]

lineLegendInput :: FrameInput Legend.Input
lineLegendInput =
  { containerId: "legend-line"
  , chart:
      { config: Legend.config
          { layout = Legend.config.layout { width = 900.0 } }
      , items:
          financialsSeries # Array.mapWithIndex \i name ->
            { label: name
            , color: seriesColor defaultTheme.categoricalPalette defaultTheme.markFill i
            , shape: Legend.SwatchLine Legend.Solid
            }
      , theme: defaultTheme
      }
  }

-- Must match `revenueVsCosts` year values in Demo.Main.
yearSeries :: Array String
yearSeries = [ "2024", "2025" ]

scatterLegendInput :: FrameInput Legend.Input
scatterLegendInput =
  { containerId: "legend-scatter"
  , chart:
      { config: Legend.config
          { layout = Legend.config.layout { width = 900.0 } }
      , items:
          yearSeries # Array.mapWithIndex \i name ->
            { label: name
            , color: seriesColor defaultTheme.categoricalPalette defaultTheme.markFill i
            , shape: Legend.SwatchMark Mark.Circle
            }
      , theme: defaultTheme
      }
  }

styleShowcaseInput :: FrameInput Legend.Input
styleShowcaseInput =
  { containerId: "legend-styles"
  , chart:
      { config: Legend.config
          { layout = Legend.config.layout { width = 900.0 }
          , itemSpacing = 24.0
          }
      , items:
          [ { label: "Circle",    color: "#1f2937", shape: Legend.SwatchMark Mark.Circle }
          , { label: "Square",    color: "#1f2937", shape: Legend.SwatchMark Mark.Square }
          , { label: "Triangle",  color: "#1f2937", shape: Legend.SwatchMark Mark.Triangle }
          , { label: "Diamond",   color: "#1f2937", shape: Legend.SwatchMark Mark.Diamond }
          , { label: "Cross",     color: "#1f2937", shape: Legend.SwatchMark Mark.Cross }
          , { label: "Star",      color: "#1f2937", shape: Legend.SwatchMark Mark.Star }
          , { label: "Solid",     color: "#1f2937", shape: Legend.SwatchLine Legend.Solid }
          , { label: "Dashed",    color: "#1f2937", shape: Legend.SwatchLine Legend.Dashed }
          , { label: "Dotted",    color: "#1f2937", shape: Legend.SwatchLine Legend.Dotted }
          , { label: "Custom",    color: "#1f2937", shape: Legend.SwatchLine (Legend.CustomDash "8 2 2 2") }
          ]
      , theme: defaultTheme
      }
  }
