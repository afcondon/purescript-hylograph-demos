-- | The second projection of the same KB: the belief chain. Nodes are
-- | facts, edges are premise citations, columns are derivation depth
-- | (axioms at 0, conclusions rightward — `Baskerville.Explain.depthOf`, the
-- | same layering the explain page and baskerville-sudoku use).
-- |
-- | One principled filter: `Dep` and `SourceLeaf` axioms are hidden —
-- | no rule ever cites them as premises (they exist for the flow
-- | projection), so this view shows state + inference while the
-- | Sankey shows structure. A selected target dims everything outside
-- | its proof cone; hovering a fact lights its premise closure.
module Viz.Beliefs
  ( beliefsTree
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Baskerville.Explain (depthOf, explain, explainFact)
import Baskerville.Kernel (Fact, FactId(..), Why(..), facts)
import Hylograph.HATS (HighlightClass(..), Tree, elem, forEach, onClick, onCoordinatedHighlight, staticStr, thunkedStr, withBehaviors) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types as E
import Make.Model (Path)
import Make.Rules (Ax(..), BuildClaim(..), BuildKB, BuildRule, governing)
import Viz.Sankey (NodeEvent(..))
import Effect (Effect)

type BeliefsInput =
  { kb :: BuildKB
  , selected :: Maybe Path
  , notify :: NodeEvent -> Effect Unit
  , sources :: Array Path -- for click-to-touch on source snapshot facts
  }

type Chip =
  { id :: Int
  , label :: String
  , x :: Number
  , y :: Number
  , w :: Number
  , fill :: String
  , stroke :: String
  , touched :: Boolean
  , path :: Maybe Path -- the single path this claim is about, if any
  }

type Wire = { from :: Int, to :: Int, x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }

chipH :: Number
chipH = 18.0

rowH :: Number
rowH = 24.0

subColW :: Number
subColW = 210.0

wrapAt :: Int
wrapAt = 16

-- | Structure axioms the rules never cite — the flow view's business.
hidden :: BuildClaim -> Boolean
hidden = case _ of
  Dep _ _ -> true
  SourceLeaf _ -> true
  _ -> false

claimColors :: BuildClaim -> { fill :: String, stroke :: String }
claimColors = case _ of
  Missing _ -> { fill: "#fdeeed", stroke: "#d64541" }
  Stale _ -> { fill: "#fff4e5", stroke: "#e08b2d" }
  Newer _ _ -> { fill: "#fff9f0", stroke: "#e08b2d" }
  Fresh _ -> { fill: "#eef3ee", stroke: "#7fa87f" }
  Phony _ -> { fill: "#ececf4", stroke: "#5b5ba6" }
  WillRun _ -> { fill: "#ececf4", stroke: "#5b5ba6" }
  _ -> { fill: "#f7f7f7", stroke: "#bbbbbb" }

claimPath :: BuildClaim -> Maybe Path
claimPath = case _ of
  Dep t _ -> Just t
  HasRecipe p -> Just p
  Phony p -> Just p
  SourceLeaf p -> Just p
  Exists p -> Just p
  Missing p -> Just p
  MTime p _ -> Just p
  Newer _ t -> Just t
  Stale p -> Just p
  Fresh p -> Just p
  WillRun p -> Just p

truncate :: Int -> String -> String
truncate n s =
  if String.length s <= n then s
  else String.take (n - 1) s <> "…"

beliefsTree :: BeliefsInput -> HATS.Tree
beliefsTree input =
  HATS.elem E.SVG
    [ F.viewBox 0.0 0.0 (totalW + 40.0) (totalH + 40.0)
    , F.preserveAspectRatio "xMidYMid meet"
    ]
    [ HATS.elem E.Group [ F.transform "translate(20,20)" ]
        [ wiresLayer <> chipsLayer <> chipLabels ]
    ]
  where
  visible = Array.filter (not <<< hidden <<< _.claim) (facts input.kb)

  depths :: Map FactId Int
  depths = depthOf input.kb

  depthFor f = fromMaybe 0 (Map.lookup f.id depths)

  maxDepth = fromMaybe 0 (maximum (visible <#> depthFor))

  byDepth :: Int -> Array (Fact BuildClaim BuildRule Ax)
  byDepth d = Array.filter (\f -> depthFor f == d) visible

  -- each depth column wraps into sub-columns of `wrapAt` chips
  subColsFor d = max 1 ((Array.length (byDepth d) + wrapAt - 1) / wrapAt)

  -- NB Array.range 0 (-1) counts DOWN ([0,-1]); guard the base case
  colX :: Int -> Number
  colX d =
    if d <= 0 then 0.0
    else Int.toNumber (Array.foldl (+) 0 (Array.range 0 (d - 1) <#> subColsFor)) * subColW

  totalW = colX maxDepth + Int.toNumber (subColsFor maxDepth) * subColW

  totalH = Int.toNumber (min wrapAt (fromMaybe 1 (maximum (Array.range 0 maxDepth <#> \d -> Array.length (byDepth d))))) * rowH

  sourceSet = Set.fromFoldable input.sources

  chips :: Array Chip
  chips = Array.range 0 maxDepth >>= \d ->
    Array.mapWithIndex
      ( \i f ->
          let
            colors = claimColors f.claim
            label = truncate 34 (show f.claim)
          in
            { id: rawId f.id
            , label
            , x: colX d + Int.toNumber (i / wrapAt) * subColW
            , y: Int.toNumber (i `mod` wrapAt) * rowH
            , w: min (subColW - 20.0) (Int.toNumber (String.length label) * 5.6 + 12.0)
            , fill: colors.fill
            , stroke: colors.stroke
            , touched: isTouched f
            , path: claimPath f.claim
            }
      )
      (byDepth d)

  rawId (FactId i) = i

  isTouched f = case f.why of
    Axiom (Touched _) -> true
    _ -> false

  chipById :: Map Int Chip
  chipById = Map.fromFoldable (chips <#> \c -> Tuple c.id c)

  wires :: Array Wire
  wires = visible >>= \f -> case f.why of
    Axiom _ -> []
    ByRule r -> r.premises # Array.mapMaybe \p ->
      case Map.lookup (rawId p) chipById, Map.lookup (rawId f.id) chipById of
        Just from, Just to -> Just
          { from: from.id
          , to: to.id
          , x1: from.x + from.w
          , y1: from.y + chipH / 2.0
          , x2: to.x
          , y2: to.y + chipH / 2.0
          }
        _, _ -> Nothing

  -- premise closure per fact, for hover; proof cone for the selection
  closureOf :: Int -> Set Int
  closureOf i = case Array.find (\f -> rawId f.id == i) visible of
    Nothing -> Set.singleton i
    Just f -> Set.fromFoldable ((explainFact input.kb f).nodes <#> rawId <<< _.id)

  cone :: Maybe (Set Int)
  cone = input.selected >>= \p ->
    governing input.kb p
      >>= \c -> explain c input.kb
        <#> \dag -> Set.fromFoldable (dag.nodes <#> rawId <<< _.id)

  inCone i = case cone of
    Nothing -> true
    Just s -> Set.member i s

  classifyChip :: Int -> String -> HATS.HighlightClass
  classifyChip i hoveredId =
    if hoveredId == "f" <> show i then HATS.Primary
    else case Int.fromString (String.drop 1 hoveredId) of
      Just h | Set.member i (closureOf h) -> HATS.Related
      _ -> HATS.Dimmed

  clickFor chip = case chip.path of
    Nothing -> []
    Just p ->
      [ HATS.onClick
          ( input.notify
              (if Set.member p sourceSet then TouchLeaf p else SelectTarget p)
          )
      ]

  wiresLayer = HATS.forEach "wires" E.Line wires (\w -> show w.from <> "-" <> show w.to) \w ->
    HATS.withBehaviors
      [ HATS.onCoordinatedHighlight
          { identify: "w" <> show w.from <> "-" <> show w.to
          , classify: \hoveredId -> case Int.fromString (String.drop 1 hoveredId) of
              Just h | Set.member w.from (closureOf h) && Set.member w.to (closureOf h) -> HATS.Related
              _ -> HATS.Dimmed
          , group: Nothing
          }
      ] $
      HATS.elem E.Line
        [ F.x1 w.x1
        , F.y1 w.y1
        , F.x2 w.x2
        , F.y2 w.y2
        , F.stroke "#999999"
        , F.strokeWidth 1.0
        , F.attr "stroke-opacity" (if inCone w.from && inCone w.to then "0.45" else "0.08")
        ]
        []

  chipsLayer = HATS.forEach "chips" E.Rect chips (show <<< _.id) \chip ->
    HATS.withBehaviors
      ( clickFor chip <>
          [ HATS.onCoordinatedHighlight
              { identify: "f" <> show chip.id
              , classify: classifyChip chip.id
              , group: Nothing
              }
          ]
      ) $
      HATS.elem E.Rect
        [ F.x chip.x
        , F.y chip.y
        , F.width chip.w
        , F.height chipH
        , F.fill chip.fill
        , F.stroke (if chip.touched then "#111111" else chip.stroke)
        , F.strokeWidth (if chip.touched then 2.0 else 1.0)
        , F.attr "rx" "4"
        , F.attr "fill-opacity" (if inCone chip.id then "1" else "0.25")
        , F.attr "stroke-opacity" (if inCone chip.id then "1" else "0.3")
        , F.style "cursor: pointer"
        ]
        []

  chipLabels = HATS.forEach "chip-labels" E.Text chips (show <<< _.id) \chip ->
    HATS.withBehaviors
      [ HATS.onCoordinatedHighlight
          { identify: "f" <> show chip.id
          , classify: classifyChip chip.id
          , group: Nothing
          }
      ] $
      HATS.elem E.Text
        [ HATS.thunkedStr "x" (show (chip.x + 6.0))
        , HATS.thunkedStr "y" (show (chip.y + chipH / 2.0))
        , HATS.staticStr "pointer-events" "none"
        , HATS.staticStr "dominant-baseline" "middle"
        , HATS.staticStr "font-size" "9.5"
        , HATS.staticStr "font-family" "'SF Mono', Menlo, monospace"
        , HATS.staticStr "fill" "#333"
        , HATS.staticStr "fill-opacity" (if inCone chip.id then "1" else "0.35")
        , HATS.thunkedStr "textContent" chip.label
        ]
        []
