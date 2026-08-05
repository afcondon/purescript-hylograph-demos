-- | Systematic catalogue of all 10 visual encoding dimensions.
-- | Each dimension varies while others are held constant.
-- | Renders using HATS (no SVG string generation, no innerHTML FFI).
module Demo.Catalogue where

import Prelude

import Data.Number (pi) as Number
import Effect (Effect)
import Effect.Console (log)
import Demo.Dom as Dom
import Hylograph.HATS (Tree, elem, forEach) as HATS
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender) as HATS
import Hylograph.Internal.Element.Types (ElementType(..))
import Onion.Hats (mkBlob, blobTree, blobTreeGrad, radialFade, linearTwoColor)
import Onion.Shape (regularPolygon, starShape, ellipseBlob, rotateAround)
import Onion.Watercolour as WC

-- =============================================================================
-- Constants
-- =============================================================================

defaultLayers :: Int
defaultLayers = 25

defaultDisp :: Number
defaultDisp = 0.15

cellW :: Number
cellW = 160.0

cx :: Number
cx = cellW / 2.0

cy :: Number
cy = cellW / 2.0

defaultWcfg :: WC.WatercolourConfig
defaultWcfg = { layers: defaultLayers, depth: 5, displacement: defaultDisp }

-- =============================================================================
-- HATS rendering helpers
-- =============================================================================

-- | Render a watercolour mark as a HATS Tree (SVG element with viewBox).
renderMark :: { shape :: Array { x :: Number, y :: Number }, color :: String
             , layers :: Int, displacement :: Number } -> Int -> HATS.Tree
renderMark cfg seed =
  let wcfg = { layers: cfg.layers, depth: 5, displacement: cfg.displacement }
      { blob } = mkBlob wcfg seed cfg.shape
  in blobTree "wc" cfg.color blob

-- | Render a row of marks into a container.
renderRow :: String -> Array { key :: String, tree :: HATS.Tree } -> Effect Unit
renderRow containerId marks = do
  let tree = HATS.forEach "marks" SVG marks _.key \m ->
        HATS.elem SVG
          [ F.viewBox 0.0 0.0 cellW cellW
          , F.attr "class" "catalogue-cell"
          ]
          [ m.tree ]
  _ <- HATS.rerender ("#" <> containerId) tree
  pure unit

-- =============================================================================
-- Catalogue
-- =============================================================================

main :: Effect Unit
main = do
  log "[Catalogue] Rendering"

  -- SIZE: radius 25 → 75
  let sizeColor = "#c25a3c"
  renderRow "dim-size" $ map (\r ->
    { key: show r
    , tree: renderMark { shape: regularPolygon cx cy r 8, color: sizeColor
                       , layers: defaultLayers, displacement: defaultDisp } 1000
    }) [ 25.0, 37.0, 50.0, 63.0, 75.0 ]

  -- SHAPE: circle, triangle, square, star, blob
  let shapeColor = "#3c7a5c"
  renderRow "dim-shape"
    [ { key: "circle", tree: renderMark { shape: regularPolygon cx cy 50.0 24, color: shapeColor
                                        , layers: defaultLayers, displacement: defaultDisp } 1100 }
    , { key: "triangle", tree: renderMark { shape: regularPolygon cx cy 50.0 3, color: shapeColor
                                          , layers: defaultLayers, displacement: defaultDisp } 1101 }
    , { key: "square", tree: renderMark { shape: regularPolygon cx cy 50.0 4, color: shapeColor
                                        , layers: defaultLayers, displacement: defaultDisp } 1102 }
    , { key: "star", tree: renderMark { shape: starShape cx cy 50.0 22.0 5, color: shapeColor
                                      , layers: defaultLayers, displacement: defaultDisp } 1103 }
    , { key: "blob", tree: renderMark { shape: (ellipseBlob { x: cx, y: cy } 55.0 42.0 0.3 12 1104).polygon
                                      , color: shapeColor, layers: defaultLayers, displacement: defaultDisp } 1105 }
    ]

  -- COLOUR: 5 from saudek palette
  renderRow "dim-colour" $ map (\c ->
    { key: c
    , tree: renderMark { shape: (ellipseBlob { x: cx, y: cy } 52.0 45.0 0.25 12 1200).polygon
                       , color: c, layers: defaultLayers, displacement: defaultDisp } 1210
    }) [ "#c25a3c", "#3c7a5c", "#7a7672", "#a8831e", "#8c5c8c" ]

  -- FUZZINESS: displacement 0.02 → 0.35
  let fuzzColor = "#a8831e"
  renderRow "dim-fuzziness" $ map (\d ->
    { key: show d
    , tree: renderMark { shape: regularPolygon cx cy 50.0 6, color: fuzzColor
                       , layers: defaultLayers, displacement: d } 1300
    }) [ 0.02, 0.08, 0.15, 0.25, 0.35 ]

  -- OPACITY: layers 5 → 40
  let opacityColor = "#8c5c8c"
  renderRow "dim-opacity" $ map (\l ->
    { key: show l
    , tree: renderMark { shape: (ellipseBlob { x: cx, y: cy } 52.0 45.0 0.25 12 1400).polygon
                       , color: opacityColor, layers: l, displacement: defaultDisp } 1410
    }) [ 5, 10, 20, 30, 40 ]

  -- GRADIENT: flat, radial, linear, rotating
  let gradShape = (ellipseBlob { x: cx, y: cy } 52.0 45.0 0.25 12 1500).polygon
      { blob: gradBlob } = mkBlob defaultWcfg 1510 gradShape

  renderRow "dim-gradient"
    [ { key: "flat"
      , tree: blobTree "flat" "#7a7672" gradBlob
      }
    , { key: "radial"
      , tree: HATS.elem Group []
          [ HATS.elem Defs [] [ radialFade "gr-radial" "#c25a3c" ]
          , blobTreeGrad "radial" "url(#gr-radial)" gradBlob
          ]
      }
    , { key: "linear"
      , tree: HATS.elem Group []
          [ HATS.elem Defs [] [ linearTwoColor "gr-linear" "#c25a3c" "#2968a8" ]
          , blobTreeGrad "linear" "url(#gr-linear)" gradBlob
          ]
      }
    , { key: "rotating"
      , tree: blobTree "rotating" "#5c6ca8" gradBlob
      }
    ]

  -- ORIENTATION: triangle at 5 angles
  let orientColor = "#2a8a8a"
  renderRow "dim-orientation" $ map (\angle ->
    { key: show angle
    , tree: renderMark { shape: rotateAround cx cy (angle * Number.pi / 180.0) (regularPolygon cx cy 50.0 3)
                       , color: orientColor, layers: defaultLayers, displacement: defaultDisp } 1600
    }) [ 0.0, 72.0, 144.0, 216.0, 288.0 ]

  -- ASPECT RATIO: wide → tall
  let aspectColor = "#c4783c"
  renderRow "dim-aspect" $ map (\p ->
    { key: show p.rx
    , tree: renderMark { shape: (ellipseBlob { x: cx, y: cy } p.rx p.ry 0.2 16 1700).polygon
                       , color: aspectColor, layers: defaultLayers, displacement: defaultDisp } 1710
    }) [ { rx: 70.0, ry: 30.0 }, { rx: 60.0, ry: 42.0 }, { rx: 50.0, ry: 50.0 }
       , { rx: 42.0, ry: 60.0 }, { rx: 30.0, ry: 70.0 } ]

  -- PALETTE: same 3-blob composition × 4 palettes
  let pb1 = (ellipseBlob { x: 65.0, y: 70.0 } 35.0 30.0 0.3 8 1800).polygon
      pb2 = (ellipseBlob { x: 105.0, y: 60.0 } 30.0 35.0 0.25 8 1801).polygon
      pb3 = (ellipseBlob { x: 85.0, y: 105.0 } 32.0 28.0 0.28 8 1802).polygon
      pw = { layers: 22, depth: 5, displacement: 0.15 }
      rpal c1 c2 c3 s =
        let { blob: b1, seed: s1 } = mkBlob pw s pb1
            { blob: b2, seed: s2 } = mkBlob pw s1 pb2
            { blob: b3 } = mkBlob pw s2 pb3
        in HATS.elem Group []
          [ blobTree "p1" c1 b1
          , blobTree "p2" c2 b2
          , blobTree "p3" c3 b3
          ]

  renderRow "dim-palette"
    [ { key: "saudek",  tree: rpal "#c25a3c" "#3c7a5c" "#a8831e" 1810 }
    , { key: "neon",    tree: rpal "#ff1493" "#00ff88" "#4444ff" 1820 }
    , { key: "primary", tree: rpal "#d42a2a" "#e8b818" "#2968a8" 1830 }
    , { key: "mono",    tree: rpal "#3a3632" "#7a7672" "#bab6b2" 1840 }
    ]

  -- COMBINATIONS: mixing 3-4 dimensions
  let comboGradShape = (ellipseBlob { x: cx, y: cy } 70.0 35.0 0.3 10 1920).polygon
      { blob: comboGradBlob } = mkBlob { layers: 25, depth: 5, displacement: 0.22 } 1925 comboGradShape

  renderRow "dim-combos"
    [ { key: "big-tri"
      , tree: renderMark { shape: regularPolygon cx cy 65.0 3, color: "#a8831e"
                         , layers: 30, displacement: 0.28 } 1900 }
    , { key: "crisp-star"
      , tree: renderMark { shape: starShape cx cy 40.0 18.0 5, color: "#ff1493"
                         , layers: 20, displacement: 0.05 } 1910 }
    , { key: "grad-ellipse"
      , tree: HATS.elem Group []
          [ HATS.elem Defs [] [ linearTwoColor "gc3" "#c25a3c" "#2a8a8a" ]
          , blobTreeGrad "combo-grad" "url(#gc3)" comboGradBlob
          ]
      }
    , { key: "rotated-sq"
      , tree: renderMark { shape: rotateAround cx cy (Number.pi / 6.0) (regularPolygon cx cy 55.0 4)
                         , color: "#3a3632", layers: 35, displacement: 0.12 } 1930 }
    ]

  -- =========================================================================
  -- Code snippets — the actual PureScript that renders each row above
  -- =========================================================================

  Dom.setCode "code-rendermark"
    ( "-- shared helper: shape + config → HATS Tree\n"
    <> "renderMark cfg seed =\n"
    <> "  let { blob } = mkBlob { layers: cfg.layers, depth: 5\n"
    <> "                         , displacement: cfg.displacement } seed cfg.shape\n"
    <> "  in blobTree \"wc\" cfg.color blob"
    )

  Dom.setCode "code-size"
    ( "-- vary radius: 25 → 75, everything else constant\n"
    <> "renderRow \"dim-size\" $ map (\\r ->\n"
    <> "  { key: show r\n"
    <> "  , tree: renderMark { shape: regularPolygon cx cy r 8\n"
    <> "                     , color: \"#c25a3c\"\n"
    <> "                     , layers: 25, displacement: 0.15 } 1000\n"
    <> "  }) [ 25.0, 37.0, 50.0, 63.0, 75.0 ]"
    )

  Dom.setCode "code-shape"
    ( "-- vary base polygon: circle(24), triangle(3), square(4), star, blob\n"
    <> "renderRow \"dim-shape\"\n"
    <> "  [ { key: \"circle\",   tree: renderMark { shape: regularPolygon cx cy 50.0 24, ... } 1100 }\n"
    <> "  , { key: \"triangle\", tree: renderMark { shape: regularPolygon cx cy 50.0  3, ... } 1101 }\n"
    <> "  , { key: \"square\",   tree: renderMark { shape: regularPolygon cx cy 50.0  4, ... } 1102 }\n"
    <> "  , { key: \"star\",     tree: renderMark { shape: starShape cx cy 50.0 22.0 5, ... } 1103 }\n"
    <> "  , { key: \"blob\",     tree: renderMark { shape: (ellipseBlob ...).polygon,   ... } 1105 }\n"
    <> "  ]"
    )

  Dom.setCode "code-colour"
    ( "-- vary fill colour, same blob shape\n"
    <> "renderRow \"dim-colour\" $ map (\\c ->\n"
    <> "  { key: c\n"
    <> "  , tree: renderMark { shape: (ellipseBlob ctr 52.0 45.0 0.25 12 1200).polygon\n"
    <> "                     , color: c, layers: 25, displacement: 0.15 } 1210\n"
    <> "  }) [ \"#c25a3c\", \"#3c7a5c\", \"#7a7672\", \"#a8831e\", \"#8c5c8c\" ]"
    )

  Dom.setCode "code-fuzziness"
    ( "-- displacement 0.02 → 0.35: crisp edges to diffuse\n"
    <> "renderRow \"dim-fuzziness\" $ map (\\d ->\n"
    <> "  { key: show d\n"
    <> "  , tree: renderMark { shape: regularPolygon cx cy 50.0 6\n"
    <> "                     , color: \"#a8831e\"\n"
    <> "                     , layers: 25, displacement: d } 1300\n"
    <> "  }) [ 0.02, 0.08, 0.15, 0.25, 0.35 ]"
    )

  Dom.setCode "code-opacity"
    ( "-- layers 5 → 40: faint wash to saturated\n"
    <> "renderRow \"dim-opacity\" $ map (\\l ->\n"
    <> "  { key: show l\n"
    <> "  , tree: renderMark { shape: (ellipseBlob ctr 52.0 45.0 ...).polygon\n"
    <> "                     , color: \"#8c5c8c\"\n"
    <> "                     , layers: l, displacement: 0.15 } 1410\n"
    <> "  }) [ 5, 10, 20, 30, 40 ]"
    )

  Dom.setCode "code-gradient"
    ( "-- flat fill vs radial gradient vs linear\n"
    <> "let { blob } = mkBlob defaultWcfg 1510 shape\n"
    <> "in renderRow \"dim-gradient\"\n"
    <> "  [ { key: \"flat\",   tree: blobTree \"flat\" \"#7a7672\" blob }\n"
    <> "  , { key: \"radial\", tree: elem Group []\n"
    <> "      [ elem Defs [] [ radialFade \"gr\" \"#c25a3c\" ]\n"
    <> "      , blobTreeGrad \"r\" \"url(#gr)\" blob ] }\n"
    <> "  , { key: \"linear\", tree: elem Group []\n"
    <> "      [ elem Defs [] [ linearTwoColor \"gl\" \"#c25a3c\" \"#2968a8\" ]\n"
    <> "      , blobTreeGrad \"l\" \"url(#gl)\" blob ] }\n"
    <> "  ]"
    )

  Dom.setCode "code-orientation"
    ( "-- rotate a triangle through 5 angles\n"
    <> "renderRow \"dim-orientation\" $ map (\\angle ->\n"
    <> "  { key: show angle\n"
    <> "  , tree: renderMark { shape: rotateAround cx cy (angle * pi / 180.0)\n"
    <> "                                (regularPolygon cx cy 50.0 3)\n"
    <> "                     , color: \"#2a8a8a\"\n"
    <> "                     , layers: 25, displacement: 0.15 } 1600\n"
    <> "  }) [ 0.0, 72.0, 144.0, 216.0, 288.0 ]"
    )

  Dom.setCode "code-aspect"
    ( "-- ellipse aspect: wide → tall\n"
    <> "renderRow \"dim-aspect\" $ map (\\{rx, ry} ->\n"
    <> "  { key: show rx\n"
    <> "  , tree: renderMark { shape: (ellipseBlob ctr rx ry 0.2 16 1700).polygon\n"
    <> "                     , color: \"#c4783c\"\n"
    <> "                     , layers: 25, displacement: 0.15 } 1710\n"
    <> "  }) [ {rx:70.0,ry:30.0}, ... , {rx:30.0,ry:70.0} ]"
    )

  Dom.setCode "code-palette"
    ( "-- same 3-blob composition, 4 different palettes\n"
    <> "let rpal c1 c2 c3 s =\n"
    <> "      let { blob: b1, seed: s1 } = mkBlob pw s blob1\n"
    <> "          { blob: b2, seed: s2 } = mkBlob pw s1 blob2\n"
    <> "          { blob: b3 }           = mkBlob pw s2 blob3\n"
    <> "      in elem Group []\n"
    <> "        [ blobTree \"p1\" c1 b1\n"
    <> "        , blobTree \"p2\" c2 b2\n"
    <> "        , blobTree \"p3\" c3 b3 ]\n"
    <> "in renderRow \"dim-palette\"\n"
    <> "  [ { key: \"saudek\",  tree: rpal \"#c25a3c\" \"#3c7a5c\" \"#a8831e\" 1810 }\n"
    <> "  , { key: \"neon\",    tree: rpal \"#ff1493\" \"#00ff88\" \"#4444ff\" 1820 }\n"
    <> "  , { key: \"primary\", tree: rpal \"#d42a2a\" \"#e8b818\" \"#2968a8\" 1830 }\n"
    <> "  , { key: \"mono\",    tree: rpal \"#3a3632\" \"#7a7672\" \"#bab6b2\" 1840 }\n"
    <> "  ]"
    )

  Dom.setCode "code-combos"
    ( "-- combine dimensions freely on single marks\n"
    <> "renderRow \"dim-combos\"\n"
    <> "  [ { key: \"big-tri\",     tree: renderMark { shape: regularPolygon cx cy 65.0 3\n"
    <> "                                            , color: \"#a8831e\", layers: 30, displacement: 0.28 } 1900 }\n"
    <> "  , { key: \"crisp-star\",  tree: renderMark { shape: starShape cx cy 40.0 18.0 5\n"
    <> "                                            , color: \"#ff1493\", layers: 20, displacement: 0.05 } 1910 }\n"
    <> "  , { key: \"grad-ellipse\", tree: elem Group []\n"
    <> "      [ elem Defs [] [ linearTwoColor \"gc3\" \"#c25a3c\" \"#2a8a8a\" ]\n"
    <> "      , blobTreeGrad \"cg\" \"url(#gc3)\" gradBlob ] }\n"
    <> "  , { key: \"rotated-sq\",  tree: renderMark { shape: rotateAround cx cy (pi/6.0) ...\n"
    <> "                                            , color: \"#3a3632\", layers: 35, displacement: 0.12 } 1930 }\n"
    <> "  ]"
    )

  log "[Catalogue] Done"
