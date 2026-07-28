-- | Demo fixtures for `Hylograph.Components.Annotation`.
-- |
-- | Two charts carry annotations:
-- |
-- | - `lineInput` — LinePlot with an `HLine` target, an `HBand` acceptable
-- |   range, a `VLine` marking a regime change, and a `Callout` on a peak.
-- | - `scatterInput` — ScatterPlot with `HLine` + `VLine` thresholds
-- |   partitioning the plane and a `Callout` labelling the outlier cell.
module Demo.Annotations
  ( lineInput
  , scatterInput
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Hylograph.Components.Annotation (Annotation(..), callout, hband, hline, vline)
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.LinePlot as LinePlot
import Hylograph.Components.LineStyle (LineStyle(..))
import Hylograph.Components.ScatterPlot as ScatterPlot
import Hylograph.Components.Theme (defaultTheme)

--------------------------------------------------------------------------------
-- Annotated LinePlot
--------------------------------------------------------------------------------

type TrendRow = { t :: Number, label :: String, value :: Number }

trend :: Array TrendRow
trend =
  [ { t: 0.0, label: "Q1-24", value: 42.0 }
  , { t: 1.0, label: "Q2-24", value: 58.0 }
  , { t: 2.0, label: "Q3-24", value: 31.0 }
  , { t: 3.0, label: "Q4-24", value: 73.0 }
  , { t: 4.0, label: "Q1-25", value: 52.0 }
  , { t: 5.0, label: "Q2-25", value: 68.0 }
  , { t: 6.0, label: "Q3-25", value: 45.0 }
  , { t: 7.0, label: "Q4-25", value: 85.0 }
  ]

lineAnnotations :: Array Annotation
lineAnnotations =
  [ HBand ((hband 40.0 60.0) { label = Just "Target band", color = Just "#2563eb" })
  , HLine ((hline 50.0) { style = Dashed, color = Just "#2563eb" })
  , VLine ((vline 3.5) { label = Just "FY25 begins", style = Dotted, color = Just "#b91c1c" })
  , Callout (callout 6.7 82.0 "↑ Q4 record")
  ]

lineInput :: FrameInput (LinePlot.Input Array (t :: Number, label :: String, value :: Number))
lineInput =
  { containerId: "annotation-line"
  , chart:
      { config:
          let c0 = LinePlot.config { x: _.t, y: _.value }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Quarter index", y: Just "Revenue (M)" }
            , annotations = lineAnnotations
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r -> r.label <> ": " <> show r.value
                }
            }
      , dataset: trend
      , theme: defaultTheme
      }
  }

--------------------------------------------------------------------------------
-- Annotated ScatterPlot
--------------------------------------------------------------------------------

type PtRow = { label :: String, x :: Number, y :: Number }

points :: Array PtRow
points =
  [ { label: "a", x: 12.0, y: 18.0 }
  , { label: "b", x: 28.0, y: 22.0 }
  , { label: "c", x: 34.0, y: 48.0 }
  , { label: "d", x: 44.0, y: 55.0 }
  , { label: "e", x: 52.0, y: 61.0 }
  , { label: "f", x: 60.0, y: 68.0 }
  , { label: "g", x: 72.0, y: 74.0 }
  , { label: "h", x: 80.0, y: 82.0 }
  , { label: "outlier", x: 18.0, y: 82.0 }
  ]

scatterAnnotations :: Array Annotation
scatterAnnotations =
  [ HLine ((hline 50.0) { label = Just "y = 50", style = Dashed })
  , VLine ((vline 50.0) { label = Just "x = 50", style = Dashed })
  , Callout (callout 18.0 78.0 "outlier ↑")
  ]

scatterInput :: FrameInput (ScatterPlot.Input Array (label :: String, x :: Number, y :: Number))
scatterInput =
  { containerId: "annotation-scatter"
  , chart:
      { config:
          let c0 = ScatterPlot.config { x: _.x, y: _.y }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "x", y: Just "y" }
            , xDomain = Just { min: 0.0, max: 100.0 }
            , yDomain = Just { min: 0.0, max: 100.0 }
            , annotations = scatterAnnotations
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r -> r.label <> " (" <> show r.x <> ", " <> show r.y <> ")"
                , radius = \_ -> 6.0
                }
            }
      , dataset: points
      , theme: defaultTheme
      }
  }
