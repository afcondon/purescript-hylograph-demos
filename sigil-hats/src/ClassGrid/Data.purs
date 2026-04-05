-- | Type class definitions for the PureScript class hierarchy grid.
-- |
-- | 24 classes across 8 groups, pre-rendered to HATS Trees.
module ClassGrid.Data
  ( ClassInfo
  , ClassEdge
  , allClasses
  , allEdges
  , groupCenterX
  , groupCenterY
  , groupLabel
  , groupColor
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Hylograph.HATS (Tree) as HTree
import Sigil.Types (RenderType)
import Sigil.Parse (parseToRenderType)
import Sigil.Hats (renderClassDeclSvg)
import Sigil.Svg.Layout.ClassDef (layoutClassDef)

type ClassInfo =
  { id :: Int
  , name :: String
  , classDeclTree :: HTree.Tree
  , width :: Number
  , height :: Number
  , group :: Int
  }

type ClassEdge = { source :: Int, target :: Int }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sig :: String -> Maybe RenderType
sig = parseToRenderType

method :: String -> String -> { name :: String, ast :: Maybe RenderType }
method n s = { name: n, ast: sig s }

-- | Method with no parseable signature (rendered as name only).
methodOnly :: String -> { name :: String, ast :: Maybe RenderType }
methodOnly n = { name: n, ast: Nothing }

type ClassDef =
  { name :: String
  , typeParams :: Array String
  , methods :: Array { name :: String, ast :: Maybe RenderType }
  }

mkClass :: Int -> Int -> ClassDef -> ClassInfo
mkClass id group def =
  let fullDef = { name: def.name, typeParams: def.typeParams, superclasses: [], methods: def.methods }
      tree = renderClassDeclSvg fullDef
      { dimensions } = layoutClassDef fullDef
  in { id, name: def.name, classDeclTree: tree, width: dimensions.width, height: dimensions.height, group }

-- ---------------------------------------------------------------------------
-- Grid layout: 2 columns × 4 rows
-- ---------------------------------------------------------------------------

-- | Column center X for a group (4 columns).
groupCenterX :: Int -> Number
groupCenterX g = case g `mod` 4 of
  0 -> -1350.0
  1 -> -450.0
  2 -> 450.0
  _ -> 1350.0

-- | Row center Y for a group (2 rows).
groupCenterY :: Int -> Number
groupCenterY g = case g `div` 4 of
  0 -> -450.0
  _ -> 450.0

-- | Muted color per group for card borders and edge strokes.
groupColor :: Int -> String
groupColor = case _ of
  0 -> "#6366f1"  -- indigo  — Functor
  1 -> "#0891b2"  -- cyan    — Semigroup
  2 -> "#d97706"  -- amber   — Eq
  3 -> "#059669"  -- emerald — Foldable
  4 -> "#dc2626"  -- red     — Alt
  5 -> "#7c3aed"  -- violet  — Semiring
  6 -> "#2563eb"  -- blue    — Category
  _ -> "#db2777"  -- pink    — Comonad

-- | Human-readable group label.
groupLabel :: Int -> String
groupLabel = case _ of
  0 -> "Functor"
  1 -> "Semigroup"
  2 -> "Eq"
  3 -> "Foldable"
  4 -> "Alt"
  5 -> "Semiring"
  6 -> "Category"
  _ -> "Comonad"

-- ---------------------------------------------------------------------------
-- Group 0: Functor tower  (ids 0–4)
-- ---------------------------------------------------------------------------

functorClasses :: Array ClassInfo
functorClasses =
  [ mkClass 0 0 { name: "Functor",     typeParams: ["f"], methods: [ method "map"   "(a -> b) -> f a -> f b" ] }
  , mkClass 1 0 { name: "Apply",       typeParams: ["f"], methods: [ method "apply" "f (a -> b) -> f a -> f b" ] }
  , mkClass 2 0 { name: "Applicative", typeParams: ["f"], methods: [ method "pure"  "a -> f a" ] }
  , mkClass 3 0 { name: "Bind",        typeParams: ["m"], methods: [ method "bind"  "m a -> (a -> m b) -> m b" ] }
  , mkClass 4 0 { name: "Monad",       typeParams: ["m"], methods: [] }
  ]

functorEdges :: Array ClassEdge
functorEdges =
  [ { source: 0, target: 1 }   -- Functor → Apply
  , { source: 1, target: 2 }   -- Apply → Applicative
  , { source: 1, target: 3 }   -- Apply → Bind
  , { source: 2, target: 4 }   -- Applicative → Monad
  , { source: 3, target: 4 }   -- Bind → Monad
  ]

-- ---------------------------------------------------------------------------
-- Group 1: Semigroup tower  (ids 5–6)
-- ---------------------------------------------------------------------------

semigroupClasses :: Array ClassInfo
semigroupClasses =
  [ mkClass 5 1 { name: "Semigroup", typeParams: ["a"], methods: [ method "append" "a -> a -> a" ] }
  , mkClass 6 1 { name: "Monoid",    typeParams: ["a"], methods: [ methodOnly "mempty" ] }
  ]

semigroupEdges :: Array ClassEdge
semigroupEdges =
  [ { source: 5, target: 6 } ]  -- Semigroup → Monoid

-- ---------------------------------------------------------------------------
-- Group 2: Eq tower  (ids 7–9)
-- ---------------------------------------------------------------------------

eqClasses :: Array ClassInfo
eqClasses =
  [ mkClass 7 2 { name: "Eq",      typeParams: ["a"], methods: [ method "eq"      "a -> a -> Boolean" ] }
  , mkClass 8 2 { name: "Ord",     typeParams: ["a"], methods: [ method "compare" "a -> a -> Ordering" ] }
  , mkClass 9 2 { name: "Bounded", typeParams: ["a"], methods: [ methodOnly "top", methodOnly "bottom" ] }
  ]

eqEdges :: Array ClassEdge
eqEdges =
  [ { source: 7, target: 8 }   -- Eq → Ord
  , { source: 8, target: 9 }   -- Ord → Bounded
  ]

-- ---------------------------------------------------------------------------
-- Group 3: Foldable tower  (ids 10–11)
-- ---------------------------------------------------------------------------

foldableClasses :: Array ClassInfo
foldableClasses =
  [ mkClass 10 3 { name: "Foldable",    typeParams: ["f"], methods: [ method "foldr" "(a -> b -> b) -> b -> f a -> b" ] }
  , mkClass 11 3 { name: "Traversable", typeParams: ["t"], methods: [ method "traverse" "(a -> m b) -> t a -> m (t b)" ] }
  ]

foldableEdges :: Array ClassEdge
foldableEdges =
  [ { source: 10, target: 11 } ]  -- Foldable → Traversable

-- ---------------------------------------------------------------------------
-- Group 4: Alt tower  (ids 12–14)
-- ---------------------------------------------------------------------------

altClasses :: Array ClassInfo
altClasses =
  [ mkClass 12 4 { name: "Alt",         typeParams: ["f"], methods: [ method "alt" "f a -> f a -> f a" ] }
  , mkClass 13 4 { name: "Plus",        typeParams: ["f"], methods: [ methodOnly "empty" ] }
  , mkClass 14 4 { name: "Alternative", typeParams: ["f"], methods: [] }
  ]

altEdges :: Array ClassEdge
altEdges =
  [ { source: 12, target: 13 }   -- Alt → Plus
  , { source: 13, target: 14 }   -- Plus → Alternative
  ]

-- ---------------------------------------------------------------------------
-- Group 5: Semiring tower  (ids 15–19)
-- ---------------------------------------------------------------------------

semiringClasses :: Array ClassInfo
semiringClasses =
  [ mkClass 15 5 { name: "Semiring",        typeParams: ["a"], methods: [ method "add" "a -> a -> a", method "mul" "a -> a -> a" ] }
  , mkClass 16 5 { name: "Ring",            typeParams: ["a"], methods: [ method "sub" "a -> a -> a" ] }
  , mkClass 17 5 { name: "CommutativeRing", typeParams: ["a"], methods: [] }
  , mkClass 18 5 { name: "EuclideanRing",   typeParams: ["a"], methods: [ method "div" "a -> a -> a", method "mod" "a -> a -> a" ] }
  , mkClass 19 5 { name: "Field",           typeParams: ["a"], methods: [] }
  ]

semiringEdges :: Array ClassEdge
semiringEdges =
  [ { source: 15, target: 16 }   -- Semiring → Ring
  , { source: 16, target: 17 }   -- Ring → CommutativeRing
  , { source: 17, target: 18 }   -- CommutativeRing → EuclideanRing
  , { source: 18, target: 19 }   -- EuclideanRing → Field
  ]

-- ---------------------------------------------------------------------------
-- Group 6: Category  (ids 20–21)
-- ---------------------------------------------------------------------------

categoryClasses :: Array ClassInfo
categoryClasses =
  [ mkClass 20 6 { name: "Semigroupoid", typeParams: ["a"], methods: [ method "compose" "a c d -> a b c -> a b d" ] }
  , mkClass 21 6 { name: "Category",     typeParams: ["a"], methods: [ method "identity" "a t t" ] }
  ]

categoryEdges :: Array ClassEdge
categoryEdges =
  [ { source: 20, target: 21 } ]  -- Semigroupoid → Category

-- ---------------------------------------------------------------------------
-- Group 7: Comonad  (ids 22–23)
-- ---------------------------------------------------------------------------

comonadClasses :: Array ClassInfo
comonadClasses =
  [ mkClass 22 7 { name: "Extend",  typeParams: ["w"], methods: [ method "extend"  "(w a -> b) -> w a -> w b" ] }
  , mkClass 23 7 { name: "Comonad", typeParams: ["w"], methods: [ method "extract" "w a -> a" ] }
  ]

comonadEdges :: Array ClassEdge
comonadEdges =
  [ { source: 22, target: 23 } ]  -- Extend → Comonad

-- ---------------------------------------------------------------------------
-- Combined
-- ---------------------------------------------------------------------------

allClasses :: Array ClassInfo
allClasses =
  functorClasses
  <> semigroupClasses
  <> eqClasses
  <> foldableClasses
  <> altClasses
  <> semiringClasses
  <> categoryClasses
  <> comonadClasses

allEdges :: Array ClassEdge
allEdges =
  functorEdges
  <> semigroupEdges
  <> eqEdges
  <> foldableEdges
  <> altEdges
  <> semiringEdges
  <> categoryEdges
  <> comonadEdges
