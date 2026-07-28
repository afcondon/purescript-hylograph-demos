-- | Demo fixtures for ScatterPlot's new mark-shape + jitter features.
-- |
-- | - `shapeInput` — scatter where `encoding.shape` maps a row's group to
-- |   one of six mark shapes (Circle / Square / Triangle / Diamond / Cross
-- |   / Star). Same dataset, same `encoding.series` so colours track groups
-- |   as well — shape and colour both vary.
-- | - `stripInput` — strip plot. One category on x (numeric index),
-- |   measurement on y, `jitter` displaces points horizontally so overlaps
-- |   fan out. A real "categorical x" axis is future work (A3); here the
-- |   numeric axis just labels the integer indices.
-- | - `shapeLegendInput` — legend matching the six shapes.
module Demo.ScatterShapes
  ( shapeInput
  , stripInput
  , shapeLegendInput
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Legend as Legend
import Hylograph.Components.Mark (MarkShape)
import Hylograph.Components.Mark as Mark
import Hylograph.Components.ScatterPlot as ScatterPlot
import Hylograph.Components.Series (seriesColor)
import Hylograph.Components.Theme (defaultTheme)

--------------------------------------------------------------------------------
-- Shape-encoded scatter
--------------------------------------------------------------------------------

type Obs = { group :: String, x :: Number, y :: Number }

-- Six groups, clustered around different 2D means. Hand-picked so every
-- mark is legible at a glance.
observations :: Array Obs
observations =
  concat
    [ cluster "α" 1.0 5.0
    , cluster "β" 3.0 6.0
    , cluster "γ" 5.0 4.5
    , cluster "δ" 2.0 2.5
    , cluster "ε" 6.0 2.0
    , cluster "ζ" 4.0 7.0
    ]
  where
  concat = Array.concat
  cluster g mx my =
    [ { group: g, x: mx - 0.4, y: my + 0.3 }
    , { group: g, x: mx + 0.2, y: my - 0.2 }
    , { group: g, x: mx - 0.1, y: my + 0.4 }
    , { group: g, x: mx + 0.4, y: my - 0.1 }
    , { group: g, x: mx,       y: my       }
    ]

shapeForGroup :: String -> MarkShape
shapeForGroup = case _ of
  "α" -> Mark.Circle
  "β" -> Mark.Square
  "γ" -> Mark.Triangle
  "δ" -> Mark.Diamond
  "ε" -> Mark.Cross
  "ζ" -> Mark.Star
  _   -> Mark.Circle

shapeInput :: FrameInput (ScatterPlot.Input Array (group :: String, x :: Number, y :: Number))
shapeInput =
  { containerId: "scatter-shapes"
  , chart:
      { config:
          let c0 = ScatterPlot.config { x: _.x, y: _.y }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "x", y: Just "y" }
            , encoding = c0.encoding
                { series = _.group
                , shape = shapeForGroup <<< _.group
                , key = \r -> r.group <> ":" <> show r.x <> "," <> show r.y
                , tooltip = \r -> r.group <> " (" <> show r.x <> ", " <> show r.y <> ")"
                , radius = \_ -> 7.0
                }
            }
      , dataset: observations
      , theme: defaultTheme
      }
  }

--------------------------------------------------------------------------------
-- Strip plot — jitter on x
--------------------------------------------------------------------------------

type StripRow = { cat :: Number, label :: String, value :: Number }

-- 4 pseudo-categories at integer x positions, ~15 points each, bell-y spread.
stripData :: Array StripRow
stripData = Array.concat
  [ cat 0.0 "A" [ 3.2, 3.8, 4.1, 4.2, 4.4, 4.5, 4.7, 4.8, 5.0, 5.1, 5.2, 5.3, 5.5, 5.9, 6.3 ]
  , cat 1.0 "B" [ 2.1, 2.9, 3.0, 3.3, 3.5, 3.6, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.3, 4.5, 5.0 ]
  , cat 2.0 "C" [ 5.0, 5.4, 5.9, 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 7.0, 7.2, 7.5, 7.9 ]
  , cat 3.0 "D" [ 1.5, 2.1, 2.4, 2.6, 2.7, 2.8, 2.9, 2.9, 3.0, 3.1, 3.2, 3.4, 3.6, 3.9, 4.4 ]
  ]
  where
  cat x lab vs =
    vs # Array.mapWithIndex \i v ->
      { cat: x, label: lab <> "·" <> show i, value: v }

stripInput :: FrameInput (ScatterPlot.Input Array (cat :: Number, label :: String, value :: Number))
stripInput =
  { containerId: "scatter-strip"
  , chart:
      { config:
          let c0 = ScatterPlot.config { x: _.cat, y: _.value }
          in c0
            { layout = c0.layout { width = 900.0 }
            , labels = { x: Just "Category (0=A 1=B 2=C 3=D)", y: Just "Value" }
            , xDomain = Just { min: -0.6, max: 3.6 }
            , yDomain = Just { min: 0.0, max: 9.0 }
            , jitter = Just { x: 22.0, y: 0.0 }
            , encoding = c0.encoding
                { key = _.label
                , tooltip = \r -> r.label <> ": " <> show r.value
                , radius = \_ -> 4.0
                }
            }
      , dataset: stripData
      , theme: defaultTheme
      }
  }

--------------------------------------------------------------------------------
-- Legend showing all six shapes
--------------------------------------------------------------------------------

shapeLegendInput :: FrameInput Legend.Input
shapeLegendInput =
  { containerId: "legend-shapes"
  , chart:
      { config: Legend.config
          { layout = Legend.config.layout { width = 900.0 }
          , itemSpacing = 24.0
          }
      , items:
          [ { label: "α",  shape: Legend.SwatchMark Mark.Circle,   color: paletteAt 0 }
          , { label: "β",  shape: Legend.SwatchMark Mark.Square,   color: paletteAt 1 }
          , { label: "γ",  shape: Legend.SwatchMark Mark.Triangle, color: paletteAt 2 }
          , { label: "δ",  shape: Legend.SwatchMark Mark.Diamond,  color: paletteAt 3 }
          , { label: "ε",  shape: Legend.SwatchMark Mark.Cross,    color: paletteAt 4 }
          , { label: "ζ",  shape: Legend.SwatchMark Mark.Star,     color: paletteAt 5 }
          ]
      , theme: defaultTheme
      }
  }
  where
  paletteAt i = seriesColor defaultTheme.categoricalPalette defaultTheme.markFill i
