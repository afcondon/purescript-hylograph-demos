-- | Gallery for hylograph-layout-components — the layout-driven half of the
-- | chart library. Sankey is the reference implementation: the first preset
-- | whose geometry comes from a `DataViz.Layout.*` algorithm rather than
-- | from scale arithmetic over the rows.
-- |
-- | Everything the page shows about Theme, Layout and Frame is inherited
-- | unchanged from `hylograph-components`. That is the claim this demo
-- | exists to make good on.
module Demo.LayoutComponents.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Hylograph.Components.Frame (FrameInput)
import Hylograph.Components.Theme (Theme, defaultTheme)
import Hylograph.LayoutComponents.Sankey (Alignment(..), LinkColor(..))
import Hylograph.LayoutComponents.Sankey as Sankey
import Type.Proxy (Proxy(..))

-- =============================================================================
-- Data — where a year's energy goes, in arbitrary units
-- =============================================================================

type FlowRow = { from :: String, to :: String, amount :: Number }

energyFlows :: Array FlowRow
energyFlows =
  [ { from: "Coal", to: "Electricity", amount: 32.0 }
  , { from: "Gas", to: "Electricity", amount: 41.0 }
  , { from: "Gas", to: "Heating", amount: 28.0 }
  , { from: "Nuclear", to: "Electricity", amount: 24.0 }
  , { from: "Wind", to: "Electricity", amount: 19.0 }
  , { from: "Solar", to: "Electricity", amount: 11.0 }
  , { from: "Oil", to: "Transport", amount: 47.0 }
  , { from: "Oil", to: "Industry", amount: 16.0 }
  , { from: "Electricity", to: "Industry", amount: 44.0 }
  , { from: "Electricity", to: "Homes", amount: 51.0 }
  , { from: "Electricity", to: "Transport", amount: 12.0 }
  , { from: "Heating", to: "Homes", amount: 28.0 }
  ]

baseConfig :: Sankey.Config (from :: String, to :: String, amount :: Number)
baseConfig =
  let c0 = Sankey.config { source: _.from, target: _.to, value: _.amount }
  in c0
    { layout = c0.layout { width = 900.0, height = 460.0 }
    , highlightGroup = Just "energy"
    , encoding = c0.encoding
        { tooltip = \r -> r.from <> " → " <> r.to <> ": " <> show r.amount <> " units" }
    }

mkInput
  :: String
  -> Sankey.Config (from :: String, to :: String, amount :: Number)
  -> Theme
  -> FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
mkInput containerId cfg theme =
  { containerId, chart: { config: cfg, dataset: energyFlows, theme } }

justifyInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
justifyInput = mkInput "sankey-justify" baseConfig defaultTheme

leftInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
leftInput = mkInput "sankey-left"
  (baseConfig { alignment = Left, layout = baseConfig.layout { width = 440.0, height = 340.0 } })
  defaultTheme

rightInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
rightInput = mkInput "sankey-right"
  (baseConfig { alignment = Right, layout = baseConfig.layout { width = 440.0, height = 340.0 } })
  defaultTheme

targetColorInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
targetColorInput = mkInput "sankey-target"
  (baseConfig { linkColor = FromTarget }) defaultTheme

uniformInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
uniformInput = mkInput "sankey-uniform"
  (baseConfig { linkColor = Uniform, linkOpacity = 0.3 }) defaultTheme

thinInput :: FrameInput (Sankey.Input Array (from :: String, to :: String, amount :: Number))
thinInput = mkInput "sankey-thin"
  (baseConfig { nodeWidth = 4.0, nodePadding = 22.0 }) defaultTheme

-- =============================================================================

type Slots =
  ( justify :: forall q. H.Slot q Void Unit
  , alignLeft :: forall q. H.Slot q Void Unit
  , alignRight :: forall q. H.Slot q Void Unit
  , targetColor :: forall q. H.Slot q Void Unit
  , uniform :: forall q. H.Slot q Void Unit
  , thin :: forall q. H.Slot q Void Unit
  )

rootComponent :: forall query input output. H.Component query input output Aff
rootComponent =
  H.mkComponent
    { initialState: const unit
    , render: \_ ->
        HH.div
          [ HP.style "font-family: Inter, Helvetica, sans-serif; padding: 32px; max-width: 1400px; margin: 0 auto;" ]
          [ HH.h1
              [ HP.style "font-size: 20px; font-weight: 500; margin: 0 0 8px; color: #111;" ]
              [ HH.text "hylograph-layout-components" ]
          , HH.p
              [ HP.style "font-size: 13px; color: #666; margin: 0 0 32px;" ]
              [ HH.text "Charts whose geometry comes from a layout algorithm rather than from scale arithmetic. Same Config / Encoding / Theme / Frame shape as hylograph-components — the sibling library changes where the coordinates come from, not how you ask for a chart." ]

          , HH.h2 [ sectionStyle ] [ HH.text "Sankey" ]
          , HH.p [ subStyle ] [ HH.text "One row of the dataset is one link: a source, a target, a quantity. Nodes are whatever names the links mention — the caller never declares them. Positions and ribbon geometry come from DataViz.Layout.Sankey; node colour comes from Theme.categoricalPalette, replacing the layout's own assignment so a Sankey sits beside a BarChart without a palette clash." ]
          , HH.p [ subStyle ] [ HH.text "Hover a node: its ribbons go Primary and its neighbours go Related, everything else Dimmed. Hover a ribbon: both its endpoints light up. That three-way classification is what a Sankey is for, and it is the same coordinated-highlight machinery the bar charts use." ]
          , HH.slot_ (Proxy :: _ "justify") unit Sankey.frame justifyInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "alignment" ]
          , HH.p [ subStyle ] [ HH.text "Where nodes sit when they have slack. Justify (above) spreads them across the full width. Left packs each node as early as its dependencies allow; Right packs them as late as possible, which reads as \"work backwards from the sinks\"." ]
          , HH.div
              [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 24px;" ]
              [ HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "alignment: Left" ]
                  , HH.slot_ (Proxy :: _ "alignLeft") unit Sankey.frame leftInput
                  ]
              , HH.div_
                  [ HH.h3 [ subHeadStyle ] [ HH.text "alignment: Right" ]
                  , HH.slot_ (Proxy :: _ "alignRight") unit Sankey.frame rightInput
                  ]
              ]

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "linkColor" ]
          , HH.p [ subStyle ] [ HH.text "Which end of a ribbon gives it its colour. FromSource (above) reads as \"where did this come from\"; FromTarget reads as \"where is this going\", which is the better choice when the interesting question is at the sinks. Uniform drops to Theme.markFill and lets the nodes carry all the colour." ]
          , HH.h3 [ subHeadStyle ] [ HH.text "linkColor: FromTarget" ]
          , HH.slot_ (Proxy :: _ "targetColor") unit Sankey.frame targetColorInput

          , HH.div [ HP.style "height: 32px;" ] []

          , HH.h3 [ subHeadStyle ] [ HH.text "linkColor: Uniform" ]
          , HH.slot_ (Proxy :: _ "uniform") unit Sankey.frame uniformInput

          , HH.div [ HP.style "height: 48px;" ] []

          , HH.h2 [ sectionStyle ] [ HH.text "nodeWidth and nodePadding" ]
          , HH.p [ subStyle ] [ HH.text "The two knobs that decide how much of the frame is flow and how much is furniture. Thin nodes with generous padding put the emphasis on the ribbons; the defaults (15 and 10) match d3-sankey, so a diagram drawn here lands where d3 would put it." ]
          , HH.slot_ (Proxy :: _ "thin") unit Sankey.frame thinInput
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
