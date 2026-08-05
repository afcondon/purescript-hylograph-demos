-- | Onion recipes — animated and compositional uses of `hylograph-onion`.
-- |
-- | The catalogue page answers *what can a mark look like*. This one answers
-- | *how does a mark behave, and how do I compose several of them* — the
-- | questions that come up building a narrated film rather than a chart.
-- |
-- | Each recipe is a technique the Polyglot video series can lift directly.
-- | Written against the real library, not a reimplementation: everything here
-- | is `Onion.*` plus HATS, and any awkwardness is a finding about the
-- | library rather than something to route around.
-- |
-- | THE CENTRAL IDEA, which the first recipe demonstrates and the rest build
-- | on: a watercolour blob is not one shape. It is an ordered stack of 20-30
-- | deformed variants at low opacity, and `Onion.Hats.blobTree` renders that
-- | stack as a KEYED DATA JOIN. So revealing a wash needs no tweening and no
-- | separate animation model — you transition the join's enter phase and the
-- | layers arrive one after another. The mark paints itself, in the order a
-- | brush would lay it down.
module Demo.Recipes where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

import Demo.Dom as Dom
import Hylograph.HATS (Tree, elem, forEachWithGUP, staticStr)
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (clearContainer, rerender) as HATS
import Hylograph.HATS.Transitions (HATSTransitions, TransitionResult(..), tickTransitions)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Transition.Types (Easing(..), TransitionConfig)
import Onion.Hats (Blob, blobTree, indexedVariants, mkBlob, variantKey)
import Onion.PathData (pathDataClosed, pathDataOpen)
import Onion.Plotter (PlotterConfig, defaultPlotter, plotLine)
import Onion.Prng as Prng
import Onion.Shape (regularPolygon)
import Onion.Watercolour as WC

-- =============================================================================
-- The series palette
-- =============================================================================

paper :: String
paper = "#f2ede2"

ink :: String
ink = "#211d16"

prussian :: String
prussian = "#254a6f"

ochre :: String
ochre = "#b3831c"

-- | Panel geometry. Every recipe draws into the same box so they can be
-- | compared without allowing for scale.
panelW :: Number
panelW = 420.0

panelH :: Number
panelH = 260.0

wcfg :: WC.WatercolourConfig
wcfg = { layers: 24, depth: 5, displacement: 0.16 }

-- =============================================================================
-- The tick loop
-- =============================================================================
--
-- One ticker for the whole page. Each recipe pushes the transitions its
-- rerender handed back onto a shared Ref; the ticker advances them all and
-- drops the ones that finish.
--
-- The step is FIXED (16ms nominal) rather than measured wall clock, and comes
-- from a timer rather than requestAnimationFrame — see `Dom.js` for why that
-- matters for anything destined to be screen-captured.

type Ticker = Ref.Ref (Array HATSTransitions)

newTicker :: Effect Ticker
newTicker = do
  active <- Ref.new []
  _ <- Dom.everyTick 16.0 \deltaMs -> do
    running <- Ref.read active
    unless (Array.null running) do
      stepped <- traverse (step deltaMs) running
      Ref.write (Array.catMaybes stepped) active
  pure active
  where
  step delta ts = do
    res <- tickTransitions delta ts
    pure case res of
      Complete -> Nothing
      Running next -> Just next

-- | Render a tree into a container and hand any transitions to the ticker.
-- |
-- | `rerender` takes a CSS SELECTOR, not an element id — passing a bare id
-- | renders nothing and reports no error. Container ids are spelled bare
-- | everywhere else here (Dom.setCode and Dom.onClick both want ids), so the
-- | `#` is added in exactly this one place.
play :: Ticker -> String -> Tree -> Effect Unit
play ticker containerId tree = do
  result <- HATS.rerender ("#" <> containerId) tree
  case result.transitions of
    Nothing -> pure unit
    Just ts -> Ref.modify_ (\a -> Array.snoc a ts) ticker

-- | Wire a replay button.
-- |
-- | Clearing first is what makes a replay a replay: a rerender against the
-- | same keys is an UPDATE, and update has no enter phase, so the layers
-- | never re-enter and nothing happens.
-- |
-- | It has to be `clearContainer`, though, and NOT a rerender of an empty
-- | tree. Plain `elem` children are positional, so an empty child list
-- | produces no exit phase and leaves every existing child in place — which
-- | looks exactly like the update problem it was meant to solve. (Written the
-- | wrong way first; the buttons did nothing and only a reload replayed.)
wireReplay :: Ticker -> String -> String -> Tree -> Effect Unit
wireReplay ticker buttonId containerId tree =
  Dom.onClick buttonId do
    HATS.clearContainer ("#" <> containerId)
    play ticker containerId tree

-- =============================================================================
-- Recipe 1 · Paint on — a wash reveals itself layer by layer
-- =============================================================================
--
-- FOR THE SERIES: every wash reveal in the film. This replaces the fade-in
-- that a static composition would use, and it costs nothing extra — the
-- layers are already there.
--
-- The template attrs are the FINAL state; the enter PhaseSpec attrs are the
-- STARTING state. (Worth stating because it is the reverse of the usual
-- from/to reading, and the type does not tell you.) Each variant's delay is
-- `staggerDelay * index`, so layer 0 lands first and layer 23 last — the
-- order a brush deposits pigment.

paintOn :: String -> Number -> Blob -> Tree
paintOn color staggerMs blob =
  forEachWithGUP "wash" Path (indexedVariants blob.variants) variantKey
    (\v ->
       elem Path
         [ F.d (pathDataClosed v.poly)
         , F.fill color
         , F.fillOpacity (show blob.opacity)   -- the TO
         , staticStr "stroke" "none"
         ]
         [])
    { enter: Just
        { attrs: [ F.fillOpacity "0" ]          -- the FROM
        , transition: Just (paintCurve staggerMs)
        }
    , update: Nothing
    , exit: Nothing
    }

-- | Pigment settles — it does not bounce and it does not ease in. A short
-- | duration per layer with a longer stagger reads as deposition; a long
-- | duration with a short stagger reads as one shape fading up, which is the
-- | thing we are trying to get away from.
paintCurve :: Number -> TransitionConfig
paintCurve staggerMs =
  { duration: Milliseconds 260.0
  , delay: Nothing
  , staggerDelay: Just staggerMs
  , easing: Just SinOut
  }

recipe1 :: Ticker -> Effect Unit
recipe1 ticker = do
  let { blob } = mkBlob wcfg 20260805 (regularPolygon 210.0 130.0 88.0 9)
      tree = stage [ paintOn prussian 42.0 blob ]
  play ticker "r1-stage" tree
  wireReplay ticker "r1-replay" "r1-stage" tree

-- =============================================================================
-- Recipe 2 · Wet on wet — two pigments that met on the paper
-- =============================================================================
--
-- FOR THE SERIES: any frame where two things meet and the meeting is the
-- point. In video 0 that is beat 7, where the wash breaches the wall.
--
-- `watercolourInterleaved` is NOT a morph between two shapes — worth saying,
-- because the name invites that reading. It alternates layer deposition
-- between two blobs, so their stacks interleave in z-order and neither sits
-- cleanly on top. Painted together while wet, rather than one over the other.
--
-- Interleaving is only visible if the two overlap, so place them accordingly.

wetOnWet :: String -> String -> Number -> WC.Polygon -> WC.Polygon -> Tree
wetOnWet colorA colorB staggerMs baseA baseB =
  let { variantsA, variantsB } = WC.watercolourInterleaved wcfg 4711 baseA baseB
      opacity = 1.0 / 24.0
      -- Zip the stacks so index order in the join IS deposition order: A0,
      -- B0, A1, B1 … Because the stagger is index-driven, that alternation
      -- is what the eye sees arriving.
      pairs = Array.concat $ Array.zipWith
                (\a b -> [ { c: colorA, poly: a }, { c: colorB, poly: b } ])
                variantsA variantsB
      keyed = Array.mapWithIndex (\i p -> { idx: i, c: p.c, poly: p.poly }) pairs
  in forEachWithGUP "wet" Path keyed (\p -> show p.idx)
       (\p ->
          elem Path
            [ F.d (pathDataClosed p.poly)
            , F.fill p.c
            , F.fillOpacity (show opacity)
            , staticStr "stroke" "none"
            ]
            [])
       { enter: Just { attrs: [ F.fillOpacity "0" ], transition: Just (paintCurve staggerMs) }
       , update: Nothing
       , exit: Nothing
       }

recipe2 :: Ticker -> Effect Unit
recipe2 ticker = do
  let a = regularPolygon 165.0 130.0 78.0 9
      b = regularPolygon 255.0 130.0 78.0 9
      tree = stage [ wetOnWet prussian ochre 26.0 a b ]
  play ticker "r2-stage" tree
  wireReplay ticker "r2-replay" "r2-stage" tree

-- =============================================================================
-- Recipe 3 · Drawn, not stroked
-- =============================================================================
--
-- FOR THE SERIES: the spine, the arrows, the wall, every box rule. The study
-- currently strokes plain SVG lines, which read as mechanical against a
-- watercolour ground. `plotLine` subdivides a polyline, displaces the
-- midpoints perpendicular to the segment, and overshoots each end — the
-- signature of a pen that was put down slightly early and lifted slightly
-- late.
--
-- Overshoot is what sells it. A wobbled line with clean ends still reads as
-- a computer being untidy; a line that runs past its corner reads as a hand.
--
-- ⚠ `wobble` IS NOT IN PIXELS, whatever the field used to claim. It scales
-- `Prng.gaussian`, whose stddev is ~0.167, so the displacement you get is
-- about `wobble / 6` px. `defaultPlotter`'s 0.8 therefore yields ±0.13 px —
-- measured, and invisible. The failure mode is nasty because a plotted line
-- then looks *exactly* like a stroked one, so it reads as "plotLine is
-- broken" rather than "that number is six times too small".
--
-- Hence the explicit config below rather than `defaultPlotter`, and hence
-- the panel showing the default alongside, so the difference is the point.

plotterVisible :: PlotterConfig
plotterVisible = { wobble: 7.0, overshoot: 2.5, subdivisions: 5 }

drawnRule :: PlotterConfig -> Prng.Seed -> Number -> Array WC.Point -> Tree
drawnRule cfg seed weight pts =
  let { points } = plotLine cfg seed pts
  in elem Path
       [ F.d (pathDataOpen points)
       , F.stroke ink
       , F.strokeWidth weight
       , F.fill "none"
       , staticStr "stroke-linecap" "round"
       ]
       []

recipe3 :: Ticker -> Effect Unit
recipe3 ticker = do
  let across y = [ { x: 40.0, y }, { x: 380.0, y } ]
      tree = stage
        [ -- the machine version, for comparison
          elem Path
            [ F.d (pathDataOpen (across 62.0))
            , F.stroke ink, F.strokeWidth 2.0, F.fill "none" ] []
        , label 40.0 48.0 "PLAIN SVG STROKE"
          -- the default config — measurably doing something, visibly nothing
        , drawnRule defaultPlotter 101 2.0 (across 112.0)
        , label 40.0 98.0 "defaultPlotter — wobble 0.8 (±0.13 px)"
          -- and three passes at a scale that reads
        , drawnRule plotterVisible 101 2.0 (across 168.0)
        , label 40.0 154.0 "wobble 7.0 — THREE SEEDS"
        , drawnRule plotterVisible 202 2.0 (across 202.0)
        , drawnRule plotterVisible 303 2.0 (across 236.0)
        ]
  play ticker "r3-stage" tree

-- =============================================================================
-- Recipe 4 · Deterministic by construction
-- =============================================================================
--
-- FOR THE SERIES: this is the one that makes a one-pass capture possible.
--
-- `Onion.Prng.Seed` is an Int and every generator is pure, so the same seed
-- gives the identical wash on every run — the film is the same film on take
-- one and take nine. The left two panels are the same call with the same
-- seed and are pixel-identical.
--
-- `Prng.split` is how you get variety without giving up that guarantee: one
-- seed becomes two independent streams, so a family of marks can differ from
-- each other while the whole family stays reproducible. The right two panels
-- come from a single split.

recipe4 :: Ticker -> Effect Unit
recipe4 ticker = do
  let shape cx = regularPolygon cx 130.0 52.0 9
      { blob: same1 } = mkBlob wcfg 777 (shape 70.0)
      { blob: same2 } = mkBlob wcfg 777 (shape 175.0)
      { left, right } = Prng.split 777
      { blob: splitL } = mkBlob wcfg left (shape 280.0)
      { blob: splitR } = mkBlob wcfg right (shape 375.0)
      tree = stage
        [ blobTree "d1" prussian same1
        , blobTree "d2" prussian same2
        , label 40.0 212.0 "SAME SEED — IDENTICAL"
        , blobTree "d3" ochre splitL
        , blobTree "d4" ochre splitR
        , label 252.0 212.0 "ONE SEED, SPLIT"
        ]
  play ticker "r4-stage" tree

-- =============================================================================
-- Recipe 5 · The stagger is the performance
-- =============================================================================
--
-- FOR THE SERIES: how to tune a reveal to the narration without redrawing
-- anything.
--
-- All three washes below are the same geometry from the same seed. Only the
-- stagger differs. A beat with 16 seconds of narration and a beat with 90
-- want visibly different deposition rates, and this is the single number that
-- buys it — which means a reveal can be retimed from the beat sheet without
-- the composition being touched.

recipe5 :: Ticker -> Effect Unit
recipe5 ticker = do
  let shape cx = regularPolygon cx 120.0 52.0 9
      mk cx = (mkBlob wcfg 31337 (shape cx)).blob
      tree = stage
        [ paintOnNamed "s1" prussian 8.0 (mk 80.0)
        , label 46.0 205.0 "8 ms — SNAPS"
        , paintOnNamed "s2" prussian 42.0 (mk 210.0)
        , label 172.0 205.0 "42 ms — SETTLES"
        , paintOnNamed "s3" prussian 110.0 (mk 340.0)
        , label 300.0 205.0 "110 ms — SOAKS"
        ]
  play ticker "r5-stage" tree
  wireReplay ticker "r5-replay" "r5-stage" tree

-- | `paintOn` with an explicit join name, so several washes can coexist in
-- | one container without sharing a selection.
paintOnNamed :: String -> String -> Number -> Blob -> Tree
paintOnNamed name color staggerMs blob =
  forEachWithGUP name Path (indexedVariants blob.variants) variantKey
    (\v ->
       elem Path
         [ F.d (pathDataClosed v.poly)
         , F.fill color
         , F.fillOpacity (show blob.opacity)
         , staticStr "stroke" "none"
         ]
         [])
    { enter: Just { attrs: [ F.fillOpacity "0" ], transition: Just (paintCurve staggerMs) }
    , update: Nothing
    , exit: Nothing
    }

-- =============================================================================
-- Furniture
-- =============================================================================

stage :: Array Tree -> Tree
stage children =
  elem SVG
    [ F.viewBox 0.0 0.0 panelW panelH
    , F.attr "class" "recipe-stage"
    ]
    children

label :: Number -> Number -> String -> Tree
label x y text =
  elem Text
    [ F.x x, F.y y
    , F.fill ink
    , F.fontSize "10px"
    , F.fontFamily "Helvetica Neue, Helvetica, Arial, sans-serif"
    , F.fontWeight "700"
    , staticStr "letter-spacing" "0.18em"
    , staticStr "opacity" "0.55"
    , staticStr "textContent" text
    ]
    []

-- =============================================================================
-- Entry
-- =============================================================================

main :: Effect Unit
main = do
  log "[Onion] recipes"
  ticker <- newTicker
  recipe1 ticker
  recipe2 ticker
  recipe3 ticker
  recipe4 ticker
  recipe5 ticker
  for_ codeSamples \s -> Dom.setCode s.id s.code

codeSamples :: Array { id :: String, code :: String }
codeSamples =
  [ { id: "code-r1"
    , code: """paintOn color staggerMs blob =
  forEachWithGUP "wash" Path (indexedVariants blob.variants) variantKey
    (\\v -> elem Path [ F.d (pathDataClosed v.poly)
                     , F.fill color
                     , F.fillOpacity (show blob.opacity) ] [])   -- the TO
    { enter: Just { attrs: [ F.fillOpacity "0" ]                 -- the FROM
                  , transition: Just (staggeredTransition (Milliseconds 260.0) 42.0) }
    , update: Nothing, exit: Nothing }"""
    }
  , { id: "code-r2"
    , code: """{ variantsA, variantsB } = watercolourInterleaved cfg seed baseA baseB

-- zip the stacks so join order IS deposition order: A0, B0, A1, B1 ...
pairs = Array.concat $ Array.zipWith
          (\\a b -> [ { c: colorA, poly: a }, { c: colorB, poly: b } ])
          variantsA variantsB"""
    }
  , { id: "code-r3"
    , code: """drawnRule cfg seed weight pts =
  let { points } = plotLine cfg seed pts
  in elem Path [ F.d (pathDataOpen points), F.stroke ink, F.fill "none" ] []

-- wobble is NOT pixels. It scales Prng.gaussian (sd ~0.167), so you get
-- about wobble/6 px. defaultPlotter's 0.8 => +/-0.13 px, measured: invisible.
plotterVisible = { wobble: 7.0, overshoot: 2.5, subdivisions: 5 }"""
    }
  , { id: "code-r4"
    , code: """{ blob: a } = mkBlob cfg 777 shape      -- same seed
{ blob: b } = mkBlob cfg 777 shape      -- identical output

{ left, right } = Prng.split 777        -- variety, still reproducible"""
    }
  , { id: "code-r5"
    , code: """-- same geometry, same seed, three staggers
paintOnNamed "s1" prussian   8.0 blob   -- snaps
paintOnNamed "s2" prussian  42.0 blob   -- settles
paintOnNamed "s3" prussian 110.0 blob   -- soaks"""
    }
  ]
