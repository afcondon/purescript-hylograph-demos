-- | Gallery for hylograph-components. Every preset the library ships, each
-- | with the prose that says what it is doing:
-- |
-- | BarChart (stacking × orientation), LinePlot (area fill, multi-series),
-- | ScatterPlot (continuous colour, mark shapes, jitter), Annotations,
-- | Histogram-via-Binning, Heatmap, BoxPlot, Treemap, Legend, PieChart.
-- |
-- | The through-line: one Axis, one Theme, one Layout, one Frame under all of
-- | them. Charts other libraries ship as separate modules are Config fields.
module Demo.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Demo.Annotations as DemoAnnotations
import Demo.BoxPlot as DemoBoxPlot
import Demo.Heatmap as DemoHeatmap
import Demo.Histogram as DemoHistogram
import Demo.Legend as DemoLegend
import Demo.ScatterShapes as DemoScatterShapes
import Demo.Treemap as DemoTreemap
import Hylograph.Components.BarChart as BarChart
import Hylograph.Components.BoxPlot as BoxPlot
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Heatmap as Heatmap
import Hylograph.Components.Legend as Legend
import Hylograph.Components.LinePlot as LinePlot
import Hylograph.Components.PieChart as PieChart
import Hylograph.Components.ScatterPlot as ScatterPlot
import Hylograph.Components.Theme (defaultTheme)
import Hylograph.Components.Treemap as Treemap
import Hylograph.Scale.Sequential (interpolateViridis)
import Type.Proxy (Proxy(..))

-- =============================================================================
-- Bar chart data
-- =============================================================================

type QuarterRow = { category :: String, value :: Number }

revenueByQuarter :: Array QuarterRow
revenueByQuarter =
  [ { category: "Q1", value: 42.0 }
  , { category: "Q2", value: 58.0 }
  , { category: "Q3", value: 31.0 }
  , { category: "Q4", value: 73.0 }
  ]

costsByQuarter :: Array QuarterRow
costsByQuarter =
  [ { category: "Q1", value: 28.0 }
  , { category: "Q2", value: 45.0 }
  , { category: "Q3", value: 22.0 }
  , { category: "Q4", value: 51.0 }
  ]

barBaseConfig :: BarChart.Config (category :: String, value :: Number)
barBaseConfig =
  let c0 = BarChart.config { category: _.category, value: _.value }
  in c0 { highlightGroup = Just "quarters" }

-- =============================================================================
-- Pie / donut data (share by quarter)
-- =============================================================================

pieInput :: FrameInput (PieChart.Input Array (category :: String, value :: Number))
pieInput =
  { containerId: "pie-share"
  , chart:
      { config:
          let c0 = PieChart.config { value: _.value, category: _.category }
          in c0
            { layout = c0.layout { width = 440.0, height = 440.0 }
            , highlightGroup = Just "quarters"
            }
      , dataset: revenueByQuarter
      , theme: defaultTheme
      }
  }

donutInput :: FrameInput (PieChart.Input Array (category :: String, value :: Number))
donutInput =
  { containerId: "donut-share"
  , chart:
      { config:
          let c0 = PieChart.config { value: _.value, category: _.category }
          in c0
            { layout = c0.layout { width = 440.0, height = 440.0 }
            , highlightGroup = Just "quarters"
            , innerRadius = 90.0
            , padAngle = 0.015
            }
      , dataset: revenueByQuarter
      , theme: defaultTheme
      }
  }

revenueInput :: FrameInput (BarChart.Input Array (category :: String, value :: Number))
revenueInput =
  { containerId: "bar-revenue"
  , chart:
      { config: barBaseConfig { labels = { x: Just "Quarter", y: Just "Revenue (M)" } }
      , dataset: revenueByQuarter
      , theme: defaultTheme
      }
  }

costsInput :: FrameInput (BarChart.Input Array (category :: String, value :: Number))
costsInput =
  { containerId: "bar-costs"
  , chart:
      { config: barBaseConfig { labels = { x: Just "Quarter", y: Just "Costs (M)" } }
      , dataset: costsByQuarter
      , theme: defaultTheme
      }
  }

-- =============================================================================
-- Multi-series data — shared by bar/line demos below
-- =============================================================================

type QuarterSeriesRow = { category :: String, series :: String, value :: Number }

financialsByQuarter :: Array QuarterSeriesRow
financialsByQuarter =
  [ { category: "Q1", series: "Revenue", value: 42.0 }
  , { category: "Q1", series: "Costs",   value: 28.0 }
  , { category: "Q2", series: "Revenue", value: 58.0 }
  , { category: "Q2", series: "Costs",   value: 45.0 }
  , { category: "Q3", series: "Revenue", value: 31.0 }
  , { category: "Q3", series: "Costs",   value: 22.0 }
  , { category: "Q4", series: "Revenue", value: 73.0 }
  , { category: "Q4", series: "Costs",   value: 51.0 }
  ]

barMultiBase :: BarChart.Config (category :: String, series :: String, value :: Number)
barMultiBase =
  let c0 = BarChart.config { category: _.category, value: _.value }
  in c0
    { encoding = c0.encoding
        { series = _.series
        , key = \r -> r.series <> "·" <> r.category
        , tooltip = \r -> r.series <> " " <> r.category <> ": " <> show r.value
        }
    , labels = { x: Just "Quarter", y: Just "M" }
    , layout = c0.layout { width = 440.0 }
    }

barGroupedInput :: FrameInput (BarChart.Input Array (category :: String, series :: String, value :: Number))
barGroupedInput =
  { containerId: "bar-grouped"
  , chart:
      { config: barMultiBase { stacking = BarChart.Grouped }
      , dataset: financialsByQuarter
      , theme: defaultTheme
      }
  }

barStackedInput :: FrameInput (BarChart.Input Array (category :: String, series :: String, value :: Number))
barStackedInput =
  { containerId: "bar-stacked"
  , chart:
      { config: barMultiBase { stacking = BarChart.Stacked }
      , dataset: financialsByQuarter
      , theme: defaultTheme
      }
  }

barNormalizedInput :: FrameInput (BarChart.Input Array (category :: String, series :: String, value :: Number))
barNormalizedInput =
  { containerId: "bar-normalized"
  , chart:
      { config: barMultiBase { stacking = BarChart.Normalized100 }
      , dataset: financialsByQuarter
      , theme: defaultTheme
      }
  }

barHorizontalInput :: FrameInput (BarChart.Input Array (category :: String, series :: String, value :: Number))
barHorizontalInput =
  { containerId: "bar-horizontal"
  , chart:
      { config: barMultiBase
          { stacking = BarChart.Stacked
          , orientation = BarChart.Horizontal
          , layout = barMultiBase.layout { width = 900.0, height = 320.0, margin = { top: 20.0, right: 20.0, bottom: 48.0, left: 100.0 } }
          , labels = { x: Just "M", y: Just "Quarter" }
          }
      , dataset: financialsByQuarter
      , theme: defaultTheme
      }
  }

type TimeSeriesRow = { t :: Number, series :: String, value :: Number }

financialsTimeSeries :: Array TimeSeriesRow
financialsTimeSeries =
  [ { t: 0.0, series: "Revenue", value: 42.0 }
  , { t: 1.0, series: "Revenue", value: 58.0 }
  , { t: 2.0, series: "Revenue", value: 31.0 }
  , { t: 3.0, series: "Revenue", value: 73.0 }
  , { t: 4.0, series: "Revenue", value: 52.0 }
  , { t: 5.0, series: "Revenue", value: 68.0 }
  , { t: 6.0, series: "Revenue", value: 45.0 }
  , { t: 7.0, series: "Revenue", value: 85.0 }
  , { t: 0.0, series: "Costs",   value: 28.0 }
  , { t: 1.0, series: "Costs",   value: 45.0 }
  , { t: 2.0, series: "Costs",   value: 22.0 }
  , { t: 3.0, series: "Costs",   value: 51.0 }
  , { t: 4.0, series: "Costs",   value: 35.0 }
  , { t: 5.0, series: "Costs",   value: 52.0 }
  , { t: 6.0, series: "Costs",   value: 30.0 }
  , { t: 7.0, series: "Costs",   value: 58.0 }
  ]

multiLineInput :: FrameInput (LinePlot.Input Array (t :: Number, series :: String, value :: Number))
multiLineInput =
  { containerId: "line-multi"
  , chart:
      { config:
          let c0 = LinePlot.config { x: _.t, y: _.value }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Quarter index", y: Just "M" }
            , encoding = c0.encoding
                { series = _.series
                , key = \r -> r.series <> "·" <> show r.t
                , tooltip = \r -> r.series <> " q" <> show r.t <> ": " <> show r.value
                }
            }
      , dataset: financialsTimeSeries
      , theme: defaultTheme
      }
  }

-- =============================================================================
-- Scatter plot data
-- =============================================================================

type TradeRow = { label :: String, year :: String, revenue :: Number, costs :: Number, volume :: Number }

revenueVsCosts :: Array TradeRow
revenueVsCosts =
  [ { label: "Q1-24", year: "2024", revenue: 42.0, costs: 28.0, volume: 12.0 }
  , { label: "Q2-24", year: "2024", revenue: 58.0, costs: 45.0, volume: 18.0 }
  , { label: "Q3-24", year: "2024", revenue: 31.0, costs: 22.0, volume: 9.0 }
  , { label: "Q4-24", year: "2024", revenue: 73.0, costs: 51.0, volume: 24.0 }
  , { label: "Q1-25", year: "2025", revenue: 52.0, costs: 35.0, volume: 14.0 }
  , { label: "Q2-25", year: "2025", revenue: 68.0, costs: 52.0, volume: 21.0 }
  , { label: "Q3-25", year: "2025", revenue: 45.0, costs: 30.0, volume: 11.0 }
  , { label: "Q4-25", year: "2025", revenue: 85.0, costs: 58.0, volume: 28.0 }
  ]

type TimePointRow = { t :: Number, label :: String, value :: Number }

revenueTrend :: Array TimePointRow
revenueTrend =
  [ { t: 0.0, label: "Q1-24", value: 42.0 }
  , { t: 1.0, label: "Q2-24", value: 58.0 }
  , { t: 2.0, label: "Q3-24", value: 31.0 }
  , { t: 3.0, label: "Q4-24", value: 73.0 }
  , { t: 4.0, label: "Q1-25", value: 52.0 }
  , { t: 5.0, label: "Q2-25", value: 68.0 }
  , { t: 6.0, label: "Q3-25", value: 45.0 }
  , { t: 7.0, label: "Q4-25", value: 85.0 }
  ]

lineInputBase :: LinePlot.Config (t :: Number, label :: String, value :: Number)
lineInputBase =
  let c0 = LinePlot.config { x: _.t, y: _.value }
  in c0
    { layout = c0.layout { width = 900.0 }
    , labels = { x: Just "Quarter index", y: Just "Revenue (M)" }
    , encoding = c0.encoding
        { key = _.label
        , tooltip = \r -> r.label <> ": " <> show r.value
        }
    }

lineInput :: FrameInput (LinePlot.Input Array (t :: Number, label :: String, value :: Number))
lineInput =
  { containerId: "line-revenue"
  , chart: { config: lineInputBase, dataset: revenueTrend, theme: defaultTheme }
  }

areaInput :: FrameInput (LinePlot.Input Array (t :: Number, label :: String, value :: Number))
areaInput =
  { containerId: "area-revenue"
  , chart:
      { config: lineInputBase
          { areaFill = Just { opacity: 0.18, baseline: 0.0 } }
      , dataset: revenueTrend
      , theme: defaultTheme
      }
  }

scatterInput :: FrameInput (ScatterPlot.Input Array (label :: String, year :: String, revenue :: Number, costs :: Number, volume :: Number))
scatterInput =
  { containerId: "scatter-rev-cost"
  , chart:
      { config:
          let c0 = ScatterPlot.config { x: _.revenue, y: _.costs }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Revenue (M)", y: Just "Costs (M)" }
            , encoding = c0.encoding
                { series = _.year
                , key = _.label
                , tooltip = \r -> r.label <> " · rev " <> show r.revenue <> "  /  cost " <> show r.costs
                , radius = \r -> r.volume / 2.0
                }
            }
      , dataset: revenueVsCosts
      , theme: defaultTheme
      }
  }

scatterContinuousInput :: FrameInput (ScatterPlot.Input Array (label :: String, year :: String, revenue :: Number, costs :: Number, volume :: Number))
scatterContinuousInput =
  { containerId: "scatter-continuous"
  , chart:
      { config:
          let c0 = ScatterPlot.config { x: _.revenue, y: _.costs }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Revenue (M)", y: Just "Costs (M)" }
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r -> r.label <> " · vol " <> show r.volume
                , radius = \_ -> 8.0
                , colorBy = Just
                    { value: _.volume
                    , interpolator: interpolateViridis
                    , domain: Nothing
                    }
                }
            }
      , dataset: revenueVsCosts
      , theme: defaultTheme
      }
  }

-- =============================================================================
-- Root
-- =============================================================================

rootComponent :: forall q i o. H.Component q i o Aff
rootComponent =
  H.mkComponent
    { initialState: \_ -> unit
    , render: \_ ->
        HH.div
          [ HP.style "font-family: Inter, Helvetica, sans-serif; padding: 32px; max-width: 1400px; margin: 0 auto;" ]
          [ HH.h1
              [ HP.style "font-size: 20px; font-weight: 500; margin: 0 0 8px; color: #111;" ]
              [ HH.text "hylograph-components" ]
          , HH.p
              [ HP.style "font-size: 13px; color: #666; margin: 0 0 32px;" ]
              [ HH.text "Seven chart presets and a Legend, over one shared substrate — the same Axis, Theme, Layout and Frame under all of them. What other libraries ship as separate modules (stacked bar, area chart, donut, histogram, strip plot) are Config fields here." ]

          , HH.h2 [ sectionStyle ] [ HH.text "BarChart" ]
          , HH.p [ subStyle ] [ HH.text "Coordinated hover: hover a bar and the matching category in the other chart highlights too (shared highlightGroup)." ]
          , HH.div
              [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 24px; margin-bottom: 48px;" ]
              [ HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "Revenue" ]
                  , HH.slot_ (Proxy :: _ "revenue") unit BarChart.frame revenueInput
                  ]
              , HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "Costs" ]
                  , HH.slot_ (Proxy :: _ "costs") unit BarChart.frame costsInput
                  ]
              ]

          , HH.h2 [ sectionStyle ] [ HH.text "LinePlot" ]
          , HH.p [ subStyle ] [ HH.text "Revenue over time. Path connects points in dataset order; points on top carry hover + tooltip." ]
          , HH.slot_ (Proxy :: _ "line") unit LinePlot.frame lineInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Same module · areaFill: Just _" ]
          , HH.p [ subStyle ] [ HH.text "Identical LinePlot.frame component, identical data, identical Config — except areaFill flips from Nothing to Just { opacity, baseline }. No AreaChart module: \"area chart\" is a style of LinePlot." ]
          , HH.slot_ (Proxy :: _ "area") unit LinePlot.frame areaInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Same module · multi-series" ]
          , HH.p [ subStyle ] [ HH.text "Same LinePlot.frame, same Config shape, but encoding.series = _.series projects a series name out of each row. Two distinct series values means two lines, coloured from Theme.categoricalPalette. A Legend preset, built from the same series names and palette, renders underneath." ]
          , HH.slot_ (Proxy :: _ "multiLine") unit LinePlot.frame multiLineInput
          , HH.slot_ (Proxy :: _ "legendLine") unit Legend.frame DemoLegend.lineLegendInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "ScatterPlot · multi-series" ]
          , HH.p [ subStyle ] [ HH.text "Revenue vs Costs across 8 quarters, grouped by year via encoding.series = _.year. Point colour comes from Theme.categoricalPalette. Radius still encodes trading volume." ]
          , HH.slot_ (Proxy :: _ "scatter") unit ScatterPlot.frame scatterInput
          , HH.slot_ (Proxy :: _ "legendScatter") unit Legend.frame DemoLegend.scatterLegendInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Continuous colour encoding" ]
          , HH.p [ subStyle ] [ HH.text "Same dataset, series dropped. encoding.colorBy maps volume through interpolateViridis (from Hylograph.Scale.Sequential) — 30+ sequential and diverging palettes are available unchanged; the component just consumes the Number → String interpolator." ]
          , HH.slot_ (Proxy :: _ "scatterContinuous") unit ScatterPlot.frame scatterContinuousInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Third categorical dimension · mark-shape encoding" ]
          , HH.p [ subStyle ] [ HH.text "encoding.shape projects a MarkShape (Circle, Square, Triangle, Diamond, Cross, Star) per row — here a row's group maps to a shape. Colour tracks the same group via encoding.series, so shape and colour are redundant but reinforcing. The paired Legend uses the same shapes as swatches via SwatchMark, keyed from Hylograph.Components.Mark." ]
          , HH.slot_ (Proxy :: _ "scatterShapes") unit ScatterPlot.frame DemoScatterShapes.shapeInput
          , HH.slot_ (Proxy :: _ "scatterShapesLegend") unit Legend.frame DemoScatterShapes.shapeLegendInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Strip plot · jitter" ]
          , HH.p [ subStyle ] [ HH.text "Same ScatterPlot.frame, four pseudo-categories at integer x positions. Config.jitter = Just { x: 22.0, y: 0.0 } fans overlapping y-values out horizontally — deterministic per point (seeded from encoding.key) so jitter stays put across rerenders. No strip-plot module." ]
          , HH.slot_ (Proxy :: _ "scatterStrip") unit ScatterPlot.frame DemoScatterShapes.stripInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "BarChart · multi-series · stacking + orientation" ]
          , HH.p [ subStyle ] [ HH.text "Same BarChart.frame, same dataset, three stacking modes. Grouped: side-by-side. Stacked: totals stacked. Normalized100: stacked + each category normalized to 100%. All via one Config field — no StackedBarChart / GroupedBarChart modules." ]
          , HH.div
              [ HP.style "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 24px; margin-bottom: 32px;" ]
              [ HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "stacking: Grouped" ]
                  , HH.slot_ (Proxy :: _ "barGrouped") unit BarChart.frame barGroupedInput
                  ]
              , HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "stacking: Stacked" ]
                  , HH.slot_ (Proxy :: _ "barStacked") unit BarChart.frame barStackedInput
                  ]
              , HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "stacking: Normalized100" ]
                  , HH.slot_ (Proxy :: _ "barNormalized") unit BarChart.frame barNormalizedInput
                  ]
              ]

          , HH.h3 [ subHeadStyle ] [ HH.text "orientation: Horizontal" ]
          , HH.p [ subStyle ] [ HH.text "Same data, same BarChart.frame, one Config field changes orientation from Vertical to Horizontal. Category axis moves from bottom to left; value axis flips to bottom. Stacked bars grow rightward." ]
          , HH.slot_ (Proxy :: _ "barHorizontal") unit BarChart.frame barHorizontalInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "Annotations" ]
          , HH.p [ subStyle ] [ HH.text "Hylograph.Components.Annotation renders reference lines, shaded bands, and text callouts through the host chart's scales. Charts with continuous x+y (ScatterPlot, LinePlot) grow an annotations Config field; categorical-x charts (BarChart) need a different context type and are future work. Bands and lines render beneath marks; labels sit on top." ]
          , HH.h3 [ subHeadStyle ] [ HH.text "LinePlot · HBand + HLine + VLine + Callout" ]
          , HH.p [ subStyle ] [ HH.text "A target band (blue HBand at 40–60), a target line (dashed HLine at 50), a regime boundary (dotted VLine at x = 3.5 marking FY25), and a Callout on the Q4 peak." ]
          , HH.slot_ (Proxy :: _ "annotationLine") unit LinePlot.frame DemoAnnotations.lineInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "ScatterPlot · threshold lines + outlier callout" ]
          , HH.p [ subStyle ] [ HH.text "Dashed HLine and VLine at 50 partition the plane into quadrants; Callout labels the outlier in the upper-left." ]
          , HH.slot_ (Proxy :: _ "annotationScatter") unit ScatterPlot.frame DemoAnnotations.scatterInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "Histogram (no module)" ]
          , HH.p [ subStyle ] [ HH.text "No Histogram module exists in this library. A histogram is a BarChart fed pre-binned rows, with barPadding = 0.0 so bars touch. The binning is the only thing Hylograph.Components.Binning contributes: equalWidth (classic) and equalCount (quantile). Dataset here is 300 synthetic samples, bimodal (main cluster near 50, secondary near 85)." ]
          , HH.h3 [ subHeadStyle ] [ HH.text "Binning.equalWidth 12" ]
          , HH.p [ subStyle ] [ HH.text "Fixed-width bins over [min, max]. Bar height = samples-per-bin. Classic histogram — shape of the distribution is legible from bar heights." ]
          , HH.slot_ (Proxy :: _ "histogramEqualWidth") unit BarChart.frame DemoHistogram.equalWidthInput

          , HH.div [ HP.style "height: 24px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "Binning.equalCount 12" ]
          , HH.p [ subStyle ] [ HH.text "Quantile bins: each bin holds roughly the same sample count, so bar heights are close to equal. The bin-range labels do the work — tight ranges where data clusters, wide ranges in sparse regions. The current BarChart has a categorical x-axis so on-screen bar widths are uniform; hover for range width. Continuous x-axis on BarChart is future work." ]
          , HH.slot_ (Proxy :: _ "histogramEqualCount") unit BarChart.frame DemoHistogram.equalCountInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "Heatmap" ]
          , HH.p [ subStyle ] [ HH.text "Grid of cells on two categorical axes, each cell coloured by a continuous value through a Number → String interpolator. Synthetic weekly activity: peaks mid-morning and mid-evening on weekdays; flatter, lower-amplitude on weekends." ]
          , HH.slot_ (Proxy :: _ "heatmap") unit Heatmap.frame DemoHeatmap.heatmapInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "BoxPlot" ]
          , HH.p [ subStyle ] [ HH.text "Per-category distribution: box spans q1–q3, median line across, whiskers extend to the farthest non-outlier within 1.5·IQR, outliers drawn as dots. Synthetic response-time distributions for four API endpoints; hover the box for summary statistics." ]
          , HH.slot_ (Proxy :: _ "boxplot") unit BoxPlot.frame DemoBoxPlot.boxPlotInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "Treemap" ]
          , HH.p [ subStyle ] [ HH.text "Rectangles packed via squarify, sized proportional to value. Nine projects coloured by domain via encoding.series; labels suppressed on cells below labelMinArea so small cells don't overflow." ]
          , HH.slot_ (Proxy :: _ "treemap") unit Treemap.frame DemoTreemap.treemapInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "Legend" ]
          , HH.p [ subStyle ] [ HH.text "Standalone preset — not a chart decorator. Same Theme/Layout/Frame shape as every other component. Swatch is one of Square, Circle, or Line (with LineStyle: Solid, Dashed, Dotted, or a CustomDash pattern). Items are supplied directly, or derived from a dataset's series projection via Legend.fromSeries / fromLineSeries so colours line up with the chart." ]
          , HH.slot_ (Proxy :: _ "legendStyles") unit Legend.frame DemoLegend.styleShowcaseInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "PieChart · Donut" ]
          , HH.p [ subStyle ] [ HH.text "Non-cartesian preset — no axes, arc paths via Hylograph.Shape.Arc. Same quarter highlightGroup as the bar charts above, so hovering a quarter coordinates across all four. Donut variant on the right uses innerRadius > 0 and a small padAngle." ]
          , HH.div
              [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 24px;" ]
              [ HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "Pie" ]
                  , HH.slot_ (Proxy :: _ "pie") unit PieChart.frame pieInput
                  ]
              , HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "Donut" ]
                  , HH.slot_ (Proxy :: _ "donut") unit PieChart.frame donutInput
                  ]
              ]
          ]
    , eval: H.mkEval H.defaultEval
    }
  where
  sectionStyle = HP.style "font-size: 16px; font-weight: 500; margin: 0 0 4px; color: #111;"
  subStyle = HP.style "font-size: 12px; color: #666; margin: 0 0 16px;"
  subHeadStyle = HP.style "font-size: 13px; font-weight: 500; margin: 0 0 8px; color: #374151;"

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  _ <- runUI rootComponent unit body
  pure unit
