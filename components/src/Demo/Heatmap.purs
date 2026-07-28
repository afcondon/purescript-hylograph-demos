-- | Demo fixtures for `Hylograph.Components.Heatmap`.
-- |
-- | Exports only data + a `FrameInput` + a container id string. The driver
-- | module (`Demo.Main`) splices the Halogen slot and surrounding HTML —
-- | this file is deliberately free of Halogen HTML and slot wiring so it
-- | can be regenerated / swapped without touching the driver.
module Demo.Heatmap
  ( ActivityRow
  , activityByDayHour
  , heatmapInput
  , containerId
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Heatmap as Heatmap
import Hylograph.Components.Theme (defaultTheme)
import Hylograph.Scale.Sequential (interpolateViridis)

type ActivityRow =
  { day :: String
  , hour :: String
  , activity :: Number
  }

daysOfWeek :: Array String
daysOfWeek = [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]

hoursOfDay :: Array String
hoursOfDay = map pad (Array.range 0 23)
  where
  pad h =
    let s = show h
    in if h < 10 then "0" <> s else s

-- | Synthetic weekly activity. Peaks mid-morning and mid-evening on
-- | weekdays; flatter and lower-amplitude on weekends. Built from piecewise
-- | quadratic "bumps" (cheaper than a full Gaussian, and no dependency on
-- | `Data.Number`) so there's a visible structure to the heatmap.
activityByDayHour :: Array ActivityRow
activityByDayHour = do
  dayIdx <- Array.range 0 (Array.length daysOfWeek - 1)
  hourIdx <- Array.range 0 (Array.length hoursOfDay - 1)
  let
    day = case Array.index daysOfWeek dayIdx of
      Just name -> name
      Nothing -> ""
    hour = case Array.index hoursOfDay hourIdx of
      Just label -> label
      Nothing -> ""
    h = Int.toNumber hourIdx
    d = Int.toNumber dayIdx
    weekend = dayIdx >= 5
    -- Two bumps: ~10:00 and ~20:00 on weekdays; broader/lower on weekends.
    weekdayShape = 60.0 * bump h 10.0 4.0 + 80.0 * bump h 20.0 4.5
    weekendShape = 30.0 * bump h 12.0 6.0 + 35.0 * bump h 22.0 5.0
    base = if weekend then weekendShape else weekdayShape
    -- Mild day-of-week ramp so Fri looks more active than Mon.
    dayRamp = if weekend then 0.0 else d * 1.5
    jitter = 4.0 * pseudoNoise (dayIdx * 31 + hourIdx * 7)
    activity = base + dayRamp + jitter
  pure { day, hour, activity }

-- | Quadratic bump: 1.0 at x == mu, smoothly 0 beyond |x - mu| >= halfWidth.
bump :: Number -> Number -> Number -> Number
bump x mu halfWidth =
  let
    z = (x - mu) / halfWidth
    z2 = z * z
  in if z2 >= 1.0 then 0.0 else 1.0 - z2

-- Cheap integer-hash-derived number in [-0.5, 0.5). Deterministic per seed.
pseudoNoise :: Int -> Number
pseudoNoise seed =
  let
    a = (seed * 2147483629) `mod` 10000
    a' = if a < 0 then a + 10000 else a
  in Int.toNumber a' / 10000.0 - 0.5

containerId :: String
containerId = "heatmap-activity"

heatmapInput :: FrameInput (Heatmap.Input Array (day :: String, hour :: String, activity :: Number))
heatmapInput =
  { containerId
  , chart:
      { config:
          let c0 = Heatmap.config { x: _.hour, y: _.day, value: _.activity }
          in c0
            { layout = c0.layout
                { width = 960.0
                , height = 360.0
                , margin = { top: 20.0, right: 20.0, bottom: 48.0, left: 60.0 }
                }
            , labels = { x: Just "Hour of day", y: Just "Day of week" }
            , encoding = c0.encoding
                { key = \r -> r.day <> "·" <> r.hour
                , tooltip = \r ->
                    r.day <> " " <> r.hour <> ":00 — " <> show r.activity
                }
            , interpolator = interpolateViridis
            , highlightGroup = Just "activity"
            }
      , dataset: activityByDayHour
      , theme: defaultTheme
      }
  }
