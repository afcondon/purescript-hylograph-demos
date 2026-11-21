-- | The lands painted: each region a watercolour wash from `hylograph-onion`,
-- | in the colour of its generation. The washes do not move, so they are drawn
-- | in their own layer under the graph and drawn again only when the mode
-- | changes; then they paint themselves, a layer at a time, one generation
-- | after another from left to right, while the people walk to them.
-- |
-- | A wash is onion's two-stage (Hobbs) blob: a rough shape deformed from the
-- | land's outline, then each layer deformed again, which gives the soft,
-- | wandering edge of pigment, and a keyed join over its layers, so the
-- | join's enter phase is the brush (onion's Recipes, "Paint on").
-- |
-- | The colours walk the hundred years: the gold of the founding, the brick
-- | and rose of the wars and the carnival, the plum of the banana years, the
-- | sky and deep blue of the last Buendías, and the green of the jungle that
-- | takes the house. Validated as a categorical palette in fixed order (the
-- | dataviz validator: all checks pass on the light surface; the gold and the
-- | rose fall under 3:1 against the paper, so every land carries its caption).
module Macondo.Wash
  ( wash
  , palette
  , hueColour
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Hylograph.Element.Types (ElementType(..))
import Data.Time.Duration (Milliseconds(..))
import Hylograph.HATS (Tree, elem, forEachWithGUP, staticStr)
import Hylograph.HATS.Friendly as F
import Macondo.Physics (Geography, Land, Mode, Shape, landsOf, stage)
import Macondo.Physics as Physics
import Hylograph.Transition.Types (Easing(..))
import Onion.Hats (Blob, indexedVariants, mkBlobHobbs, variantKey)
import Onion.PathData (pathDataClosed)

palette :: Array String
palette = [ "#E3A21A", "#B8402A", "#E07A9A", "#7A2E6E", "#4F8FD6", "#2B4C9A", "#2E9A5E" ]

-- | Lands with no generation (the worlds) are washed in ink.
hueColour :: Maybe Int -> String
hueColour = case _ of
  Just index -> fromMaybe ink (Array.index palette index)
  Nothing -> ink

ink :: String
ink = "#4E555A"

wash :: Geography -> Mode -> Tree
wash atlas mode =
  -- Each wash's join sits directly under the SVG. Nested inside an element
  -- that is itself entering (a keyed Group per land, as this was first
  -- written), its enter transitions are dropped by hylograph-selection 0.6.0
  -- (`renderEnteringItem` keeps the children's selections, not their
  -- transitions), and the paint never comes up.
  elem SVG [ F.viewBox 0.0 0.0 stage.width stage.height, F.class_ "washes" ]
    ( landsOf atlas mode # Array.mapWithIndex \index land ->
        paintOn ("wash-" <> show index) (hueColour land.hue) (startOf land) (blobOf index land)
    )

-- | A wash that paints itself: each layer enters from nothing, one
-- | `perLayer` after the last, and the whole wash waits its generation's turn.
paintOn :: String -> String -> Number -> Blob -> Tree
paintOn name colour start blob =
  forEachWithGUP name Path (indexedVariants blob.variants) variantKey
    ( \layer -> elem Path
        [ F.d (pathDataClosed layer.poly)
        , F.fill colour
        , F.fillOpacity (show blob.opacity) -- where the layer ends
        , staticStr "stroke" "none"
        ]
        []
    )
    { enter: Just
        { attrs: [ F.fillOpacity "0" ] -- where it starts
        , transition: Just
            { duration: Milliseconds 140.0
            , delay: Just (Milliseconds start)
            , staggerDelay: Just perLayer
            , easing: Just SinOut
            }
        }
    , update: Nothing
    , exit: Nothing
    }

-- | The painting of one mode takes about two seconds, under half the walk:
-- | each generation starts `perGeneration` after the one to its left, and
-- | lays its `layers` down `perLayer` apart, so the lands are there before
-- | the people arrive.
perGeneration :: Number
perGeneration = 160.0

perLayer :: Number
perLayer = 22.0

startOf :: Land -> Number
startOf land = perGeneration * toNumber (fromMaybe 0 land.hue)

layers :: Int
layers = 30

-- | A land's outline, deformed and stacked. Each pass doubles the points, so
-- | depth is the cost: at 4 + 3 passes on a 24-point outline the grid carried
-- | 1.7 million path points (27 MB of path data), and Safari, which
-- | recomposites the layer under the graph on every tick, slowed to a crawl.
-- | At 3 + 2 on 12 points it is about a tenth of that. The seed is the land's index, so
-- | the same land is painted the same way on every load.
blobOf :: Int -> Land -> Blob
blobOf index land = (mkBlobHobbs
  { layers, baseDepth: 3, baseDisplacement: 0.3, layerDepth: 2, layerDisplacement: 0.35 }
  (7919 * (index + 1))
  (outline land.shape)).blob

-- | The shape a land is painted from: the outline the forces hold it to, at
-- | half its points (`Macondo.Physics.paintOutline`), so the paint and the
-- | land agree.
outline :: Shape -> Array { x :: Number, y :: Number }
outline = Physics.paintOutline
