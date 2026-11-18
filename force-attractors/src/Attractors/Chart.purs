-- | The picture, as functions from data to HATS trees. Nothing here has an
-- | effect of its own: `Attractors.Main` hands these trees to the interpreter.
module Attractors.Chart
  ( Body
  , Reading
  , nodesChart
  , controls
  , readout
  , radiusOf
  ) where

import Prelude

import Attractors.Flare (NodePath(..))
import Attractors.Layout (Arrangement, Guide(..), Layout, allLayouts, label, stage)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Element.Types (ElementType(..))
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.HATS (Tree, elem, forEach, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F

-- | A node of the hierarchy as the simulation carries it.
type Body = SimulationNode
  ( path :: NodePath
  , parent :: Maybe NodePath
  , radius :: Number
  , branch :: Maybe Int
  )

-- | A disc's radius from the node's value: area in proportion, within limits
-- | that keep the smallest leaf visible and the root from covering its
-- | children.
radiusOf :: Number -> Number
radiusOf value = clamp 2.0 7.0 (1.6 + sqrt value / 45.0)

-- ============================================================================
-- The nodes
-- ============================================================================

nodesChart :: Arrangement -> Array Body -> Tree
nodesChart arrangement bodies =
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height ]
    [ elem Group [ F.class_ "guides" ]
        [ forEach "rings" Circle rings _.key \{ shape } ->
            elem Circle [ F.cx shape.x, F.cy shape.y, F.r shape.radius ] []
        , forEach "boxes" Rect boxes _.key \{ shape } ->
            elem Rect [ F.x shape.x, F.y shape.y, F.width shape.width, F.height shape.height ] []
        , forEach "outlines" Path outlines _.key \{ shape } ->
            elem Path [ F.d shape.path, F.transform (translate shape.at) ] []
        ]
    , elem Group [ F.class_ "links" ]
        [ forEach "links" Line links _.key \link ->
            elem Line [ F.x1 link.from.x, F.y1 link.from.y, F.x2 link.to.x, F.y2 link.to.y ] []
        ]
    , elem Group [ F.class_ "nodes" ]
        [ forEach "nodes" Circle bodies pathOf \body ->
            elem Circle
              [ F.cx body.x
              , F.cy body.y
              , F.r body.radius
              , F.fill (colourOf body.branch)
              , F.attr "data-path" (pathOf body)
              ]
              []
        ]
    ]
  where
  rings = withKeys $ arrangement.guides # Array.mapMaybe case _ of
    Ring ring -> Just ring
    _ -> Nothing
  boxes = withKeys $ arrangement.guides # Array.mapMaybe case _ of
    Box box -> Just box
    _ -> Nothing
  outlines = withKeys $ arrangement.guides # Array.mapMaybe case _ of
    Outline outline -> Just outline
    _ -> Nothing

  byPath = Map.fromFoldable (bodies <#> \body -> Tuple body.path body)
  links =
    if arrangement.linked then bodies # Array.mapMaybe \body -> do
      parent <- body.parent >>= flip Map.lookup byPath
      pure { key: pathOf body, from: { x: parent.x, y: parent.y }, to: { x: body.x, y: body.y } }
    else []

pathOf :: Body -> String
pathOf body = case body.path of
  NodePath path -> path

-- | Guides have no identity beyond their place in the layout's output.
withKeys :: forall shape. Array shape -> Array { key :: String, shape :: shape }
withKeys = Array.mapWithIndex \index shape -> { key: show index, shape }

translate :: { x :: Number, y :: Number } -> String
translate point = "translate(" <> show point.x <> "," <> show point.y <> ")"

-- | Ink for the root, one colour for each of its children's subtrees.
colourOf :: Maybe Int -> String
colourOf = case _ of
  Nothing -> ink
  Just branch -> fromMaybe ink (Array.index palette (branch `mod` Array.length palette))

ink :: String
ink = "#14181A"

palette :: Array String
palette =
  [ "#14548C" -- blue
  , "#A81D22" -- red
  , "#2F7D5B" -- green
  , "#B8791F" -- ochre
  , "#6B4E9B" -- violet
  , "#1F8A99" -- teal
  , "#B5506E" -- rose
  , "#5E6B2E" -- olive
  , "#8A5A3C" -- umber
  , "#4E555A" -- slate
  ]

-- ============================================================================
-- The buttons and the readout
-- ============================================================================

-- | One button for each layout; the current one is marked.
controls :: (Layout -> Effect Unit) -> Layout -> Tree
controls choose current =
  elem Div [ F.class_ "buttons" ]
    [ forEach "layouts" Button allLayouts label \layout ->
        withBehaviors [ onClick (choose layout) ] $
          elem Button
            [ F.class_ (if layout == current then "button current" else "button")
            , F.attr "data-layout" (label layout)
            , staticStr "textContent" (label layout)
            ]
            []
    ]

-- | What the kernel says about the motion, and how far the picture is from
-- | the layout it is heading for.
type Reading =
  { layout :: Layout
  , ticks :: Int -- since the last change of layout
  , kineticEnergy :: Number
  , maxDisplacement :: Number -- the furthest any node moved in the last tick
  , maxAcceleration :: Number -- the most any node's step changed between the last two ticks
  , furthestFromTarget :: Number
  , settled :: Boolean
  }

readout :: Reading -> Tree
readout reading =
  elem Div
    [ F.class_ "figures"
    , F.attr "data-settled" (show reading.settled)
    , F.attr "data-ticks" (show reading.ticks)
    , F.attr "data-layout" (label reading.layout)
    ]
    [ figure "kinetic energy" (decimal 2 reading.kineticEnergy)
    , figure "furthest step" (decimal 3 reading.maxDisplacement)
    , figure "acceleration" (decimal 4 reading.maxAcceleration)
    , figure "furthest from target" (decimal 2 reading.furthestFromTarget)
    , figure "ticks" (show reading.ticks)
    , figure "state" (if reading.settled then "at rest" else "in flight")
    ]
  where
  figure name value =
    elem Div [ F.class_ "figure" ]
      [ elem Span [ F.class_ "name", staticStr "textContent" name ] []
      , elem Span [ F.class_ "value", staticStr "textContent" value ] []
      ]
  decimal places = toStringWith (fixed places)
