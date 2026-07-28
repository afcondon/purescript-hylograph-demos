-- | Demo fixtures for `Hylograph.Components.Treemap`.
-- |
-- | Exports only data + a `FrameInput` + a container id string. The driver
-- | module (`Demo.Main`) splices the Halogen slot and surrounding HTML —
-- | this file is deliberately free of Halogen HTML and slot wiring so it
-- | can be regenerated / swapped without touching the driver.
-- |
-- | Dataset: nine "projects" with a size metric in lines of code, spanning
-- | ~3 orders of magnitude so the squarified cell sizes vary visibly.
-- | Grouped by domain (`domain :: String`) so a caller can flip on
-- | multi-series colouring by passing `encoding.series = _.domain`.
module Demo.Treemap
  ( ProjectRow
  , projectSizes
  , treemapInput
  , containerId
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Theme (defaultTheme)
import Hylograph.Components.Treemap as Treemap

type ProjectRow =
  { name :: String
  , domain :: String
  , loc :: Number
  }

-- | Synthetic project-size dataset. Values span ~20 to ~32000 so the
-- | treemap shows a wide range of cell areas.
projectSizes :: Array ProjectRow
projectSizes =
  [ { name: "hylograph-selection", domain: "viz", loc: 31800.0 }
  , { name: "hylograph-components", domain: "viz", loc: 4200.0 }
  , { name: "hylograph-demos", domain: "viz", loc: 1800.0 }
  , { name: "minard-api", domain: "apps", loc: 12500.0 }
  , { name: "shaped-steer", domain: "apps", loc: 8300.0 }
  , { name: "tarot-music", domain: "music", loc: 2700.0 }
  , { name: "msm", domain: "music", loc: 6400.0 }
  , { name: "worklog-server", domain: "infra", loc: 820.0 }
  , { name: "polyglot-deploy", domain: "infra", loc: 140.0 }
  ]

containerId :: String
containerId = "treemap-project-sizes"

treemapInput
  :: FrameInput
       ( Treemap.Input Array
           (name :: String, domain :: String, loc :: Number)
       )
treemapInput =
  { containerId
  , chart:
      { config:
          let c0 = Treemap.config { value: _.loc, label: _.name }
          in c0
            { layout = c0.layout
                { width = 720.0
                , height = 420.0
                , margin = { top: 12.0, right: 12.0, bottom: 12.0, left: 12.0 }
                }
            , encoding = c0.encoding
                { series = _.domain
                , tooltip = \r ->
                    r.name <> " (" <> r.domain <> "): "
                      <> show r.loc <> " LOC"
                }
            , highlightGroup = Just "projects"
            }
      , dataset: projectSizes
      , theme: defaultTheme
      }
  }
