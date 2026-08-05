-- | Les Misérables character co-occurrence network with pen-plotter rendering
-- | and watercolour cluster blobs.
-- |
-- | Uses hylograph-simulation for the force layout, Halogen for the component
-- | lifecycle, and SVG filters for watercolour effects.
module Demo.LesMis where

import Prelude

import Data.Array as Array
import Data.Array (nub)
import Data.Foldable (foldl)
import Data.Int (toNumber, floor) as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Number (sqrt) as Number
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import Hylograph.Simulation (runSimulation, Engine(..), SimulationHandle, SimulationEvent(..))
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.ForceEngine.Setup (setup, manyBody, collide, link, center, withStrength, withRadius, withDistance, static)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Demo.Dom as Dom
import Onion.PathData (pathDataClosed)
import Onion.Prng as Prng
import Onion.Shape (ellipseBlob)
import Onion.Watercolour as WC

-- =============================================================================
-- Types
-- =============================================================================

type NodeRow = (name :: String, group :: Int)
type LesMisNode = { id :: Int, name :: String, group :: Int }
type LesMisLink = { source :: Int, target :: Int, value :: Int }

-- | Visual constants (tuned interactively, now baked in)
edgeWidth :: Number
edgeWidth = 0.6

edgeOpacity :: Number
edgeOpacity = 0.55

wobble :: Number
wobble = 1.2

nodeScale :: Number
nodeScale = 1.1

-- | Pre-computed watercolour blob per group (stable across ticks)
type GroupBlob =
  { group :: Int
  , variants :: Array (Array { x :: Number, y :: Number })
  , opacity :: Number
  }

type State =
  { nodes :: Array (SimulationNode NodeRow)
  , links :: Array LesMisLink
  , handle :: Maybe (SimulationHandle NodeRow)
  , blobs :: Array GroupBlob
  , centerNodes :: Array Int
  }

data Action
  = Initialize
  | SimTick Number
  | SimDone

-- =============================================================================
-- Cluster computation
-- =============================================================================

type Cluster =
  { group :: Int
  , cx :: Number
  , cy :: Number
  , radius :: Number
  , count :: Int
  }

computeClusters :: Array (SimulationNode NodeRow) -> Array Cluster
computeClusters nodes =
  let groups = nub $ map _.group nodes
  in Array.mapMaybe (clusterForGroup nodes) groups

clusterForGroup
  :: Array (SimulationNode NodeRow) -> Int -> Maybe Cluster
clusterForGroup nodes g =
  let members = Array.filter (\n -> n.group == g) nodes
      count = Array.length members
  in if count == 0 then Nothing
     else
       let n = Int.toNumber count
           cx = foldl (\acc m -> acc + m.x) 0.0 members / n
           cy = foldl (\acc m -> acc + m.y) 0.0 members / n
           maxDist = foldl (\acc m ->
             let dx = m.x - cx
                 dy = m.y - cy
             in max acc (Number.sqrt (dx * dx + dy * dy))
           ) 0.0 members
           radius = maxDist + 30.0
       in Just { group: g, cx, cy, radius, count }

-- =============================================================================
-- Pre-computation
-- =============================================================================

-- | Generate one watercolour blob per group. Base shape is an ellipse
-- | at origin with reference radius ~60. Positioned and scaled at render time.
precomputeBlobs :: Array GroupBlob
precomputeBlobs =
  let groups = nub $ map _.group lesMisNodes
      cfg = { layers: 20, depth: 5, displacement: 0.15 }
      opacity = 1.0 / 20.0
  in map (\g ->
    let seed = g * 7919 + 42
        shape = (ellipseBlob { x: 0.0, y: 0.0 } 60.0 50.0 0.25 12 seed).polygon
        wc = WC.watercolourBlob cfg (seed + 1000) shape
    in { group: g, variants: wc.variants, opacity }
  ) groups

-- | Find the highest-degree node in each group (the cluster center).
findCenterNodes :: Array LesMisLink -> Array Int
findCenterNodes links =
  let groups = nub $ map _.group lesMisNodes
      degreeOf nid = Array.length (Array.filter (\l -> l.source == nid || l.target == nid) links)
  in Array.mapMaybe (\g ->
    let members = Array.filter (\n -> n.group == g) lesMisNodes
    in case Array.sortBy (\a b -> compare (degreeOf b.id) (degreeOf a.id)) members of
      [] -> Nothing
      sorted -> case Array.head sorted of
        Just n -> Just n.id
        Nothing -> Nothing
  ) groups

-- =============================================================================
-- Dataset
-- =============================================================================

lesMisNodes :: Array LesMisNode
lesMisNodes =
  [ {id:0,name:"Myriel",group:1},{id:1,name:"Napoleon",group:1},{id:2,name:"Mlle.Baptistine",group:1}
  , {id:3,name:"Mme.Magloire",group:1},{id:4,name:"CountessdeLo",group:1},{id:5,name:"Geborand",group:1}
  , {id:6,name:"Champtercier",group:1},{id:7,name:"Cravatte",group:1},{id:8,name:"Count",group:1}
  , {id:9,name:"OldMan",group:1},{id:10,name:"Labarre",group:2},{id:11,name:"Valjean",group:2}
  , {id:12,name:"Marguerite",group:3},{id:13,name:"Mme.deR",group:2},{id:14,name:"Isabeau",group:2}
  , {id:15,name:"Gervais",group:2},{id:16,name:"Tholomyes",group:3},{id:17,name:"Listolier",group:3}
  , {id:18,name:"Fameuil",group:3},{id:19,name:"Blacheville",group:3},{id:20,name:"Favourite",group:3}
  , {id:21,name:"Dahlia",group:3},{id:22,name:"Zephine",group:3},{id:23,name:"Fantine",group:3}
  , {id:24,name:"Mme.Thenardier",group:4},{id:25,name:"Thenardier",group:4},{id:26,name:"Cosette",group:5}
  , {id:27,name:"Javert",group:4},{id:28,name:"Fauchelevent",group:0},{id:29,name:"Bamatabois",group:2}
  , {id:30,name:"Perpetue",group:3},{id:31,name:"Simplice",group:2},{id:32,name:"Scaufflaire",group:2}
  , {id:33,name:"Woman1",group:2},{id:34,name:"Judge",group:2},{id:35,name:"Champmathieu",group:2}
  , {id:36,name:"Brevet",group:2},{id:37,name:"Chenildieu",group:2},{id:38,name:"Cochepaille",group:2}
  , {id:39,name:"Pontmercy",group:4},{id:40,name:"Boulatruelle",group:6},{id:41,name:"Eponine",group:4}
  , {id:42,name:"Anzelma",group:4},{id:43,name:"Woman2",group:5},{id:44,name:"MotherInnocent",group:0}
  , {id:45,name:"Gribier",group:0},{id:46,name:"Jondrette",group:7},{id:47,name:"Mme.Burgon",group:7}
  , {id:48,name:"Gavroche",group:8},{id:49,name:"Gillenormand",group:5},{id:50,name:"Magnon",group:5}
  , {id:51,name:"Mlle.Gillenormand",group:5},{id:52,name:"Mme.Pontmercy",group:5}
  , {id:53,name:"Mlle.Vaubois",group:5},{id:54,name:"Lt.Gillenormand",group:5}
  , {id:55,name:"Marius",group:8},{id:56,name:"BaronessT",group:5},{id:57,name:"Mabeuf",group:8}
  , {id:58,name:"Enjolras",group:8},{id:59,name:"Combeferre",group:8},{id:60,name:"Prouvaire",group:8}
  , {id:61,name:"Feuilly",group:8},{id:62,name:"Courfeyrac",group:8},{id:63,name:"Bahorel",group:8}
  , {id:64,name:"Bossuet",group:8},{id:65,name:"Joly",group:8},{id:66,name:"Grantaire",group:8}
  , {id:67,name:"MotherPlutarch",group:9},{id:68,name:"Gueulemer",group:4},{id:69,name:"Babet",group:4}
  , {id:70,name:"Claquesous",group:4},{id:71,name:"Montparnasse",group:4},{id:72,name:"Toussaint",group:5}
  , {id:73,name:"Child1",group:10},{id:74,name:"Child2",group:10},{id:75,name:"Brujon",group:4}
  , {id:76,name:"Mme.Hucheloup",group:8}
  ]

lesMisLinks :: Array LesMisLink
lesMisLinks =
  [ {source:1,target:0,value:1},{source:2,target:0,value:8},{source:3,target:0,value:10},{source:3,target:2,value:6}
  , {source:4,target:0,value:1},{source:5,target:0,value:1},{source:6,target:0,value:1},{source:7,target:0,value:1}
  , {source:8,target:0,value:2},{source:9,target:0,value:1},{source:11,target:10,value:1},{source:11,target:3,value:3}
  , {source:11,target:2,value:3},{source:11,target:0,value:5},{source:12,target:11,value:1},{source:13,target:11,value:1}
  , {source:14,target:11,value:1},{source:15,target:11,value:1},{source:17,target:16,value:4},{source:18,target:16,value:4}
  , {source:18,target:17,value:4},{source:19,target:16,value:4},{source:19,target:17,value:4},{source:19,target:18,value:4}
  , {source:20,target:16,value:3},{source:20,target:17,value:3},{source:20,target:18,value:3},{source:20,target:19,value:4}
  , {source:21,target:16,value:3},{source:21,target:17,value:3},{source:21,target:18,value:3},{source:21,target:19,value:3}
  , {source:21,target:20,value:5},{source:22,target:16,value:3},{source:22,target:17,value:3},{source:22,target:18,value:3}
  , {source:22,target:19,value:3},{source:22,target:20,value:4},{source:22,target:21,value:4},{source:23,target:16,value:3}
  , {source:23,target:17,value:3},{source:23,target:18,value:3},{source:23,target:19,value:3},{source:23,target:20,value:4}
  , {source:23,target:21,value:4},{source:23,target:22,value:4},{source:23,target:12,value:2},{source:23,target:11,value:9}
  , {source:24,target:23,value:2},{source:24,target:11,value:7},{source:25,target:24,value:13},{source:25,target:23,value:1}
  , {source:25,target:11,value:12},{source:26,target:24,value:4},{source:26,target:11,value:31},{source:26,target:16,value:1}
  , {source:26,target:25,value:1},{source:27,target:11,value:17},{source:27,target:23,value:5},{source:27,target:25,value:5}
  , {source:27,target:24,value:1},{source:27,target:26,value:1},{source:28,target:11,value:8},{source:28,target:27,value:1}
  , {source:29,target:23,value:1},{source:29,target:27,value:1},{source:29,target:11,value:2},{source:30,target:23,value:1}
  , {source:31,target:30,value:2},{source:31,target:11,value:3},{source:31,target:23,value:2},{source:31,target:27,value:1}
  , {source:32,target:11,value:1},{source:33,target:11,value:2},{source:33,target:27,value:1},{source:34,target:11,value:3}
  , {source:34,target:29,value:2},{source:35,target:11,value:3},{source:35,target:34,value:3},{source:35,target:29,value:2}
  , {source:36,target:34,value:2},{source:36,target:35,value:2},{source:36,target:11,value:2},{source:37,target:34,value:2}
  , {source:37,target:35,value:2},{source:37,target:36,value:2},{source:37,target:11,value:2},{source:38,target:34,value:2}
  , {source:38,target:35,value:2},{source:38,target:36,value:2},{source:38,target:37,value:2},{source:38,target:11,value:2}
  , {source:39,target:25,value:1},{source:40,target:25,value:1},{source:41,target:24,value:2},{source:41,target:25,value:3}
  , {source:42,target:41,value:2},{source:42,target:25,value:2},{source:42,target:24,value:1},{source:43,target:11,value:3}
  , {source:43,target:26,value:1},{source:43,target:27,value:1},{source:44,target:28,value:3},{source:44,target:11,value:1}
  , {source:45,target:28,value:2},{source:47,target:46,value:1},{source:48,target:47,value:2},{source:48,target:25,value:1}
  , {source:48,target:27,value:1},{source:48,target:11,value:1},{source:49,target:26,value:3},{source:49,target:11,value:2}
  , {source:50,target:49,value:1},{source:51,target:49,value:9},{source:51,target:26,value:2},{source:51,target:11,value:2}
  , {source:52,target:51,value:1},{source:52,target:39,value:1},{source:53,target:51,value:1},{source:54,target:51,value:2}
  , {source:54,target:49,value:1},{source:54,target:26,value:1},{source:55,target:51,value:6},{source:55,target:49,value:12}
  , {source:55,target:39,value:1},{source:55,target:54,value:1},{source:55,target:26,value:21},{source:55,target:11,value:19}
  , {source:55,target:16,value:1},{source:55,target:25,value:2},{source:55,target:41,value:5},{source:55,target:48,value:4}
  , {source:56,target:49,value:1},{source:56,target:55,value:1},{source:57,target:55,value:1},{source:57,target:41,value:7}
  , {source:57,target:48,value:1},{source:57,target:11,value:2},{source:58,target:55,value:7},{source:58,target:48,value:6}
  , {source:58,target:27,value:6},{source:58,target:57,value:1},{source:58,target:11,value:4},{source:59,target:58,value:15}
  , {source:59,target:55,value:5},{source:59,target:48,value:6},{source:59,target:57,value:2},{source:60,target:48,value:1}
  , {source:60,target:58,value:4},{source:60,target:59,value:2},{source:61,target:48,value:2},{source:61,target:58,value:6}
  , {source:61,target:60,value:2},{source:61,target:59,value:5},{source:61,target:57,value:1},{source:61,target:55,value:1}
  , {source:62,target:55,value:9},{source:62,target:58,value:17},{source:62,target:59,value:13},{source:62,target:48,value:7}
  , {source:62,target:57,value:2},{source:62,target:41,value:1},{source:62,target:61,value:6},{source:62,target:60,value:3}
  , {source:63,target:59,value:5},{source:63,target:48,value:5},{source:63,target:62,value:6},{source:63,target:57,value:2}
  , {source:63,target:58,value:4},{source:63,target:61,value:3},{source:63,target:60,value:2},{source:63,target:55,value:1}
  , {source:64,target:55,value:5},{source:64,target:62,value:12},{source:64,target:48,value:5},{source:64,target:63,value:4}
  , {source:64,target:58,value:10},{source:64,target:59,value:6},{source:64,target:57,value:1},{source:64,target:11,value:1}
  , {source:64,target:60,value:1},{source:65,target:63,value:5},{source:65,target:64,value:7},{source:65,target:48,value:3}
  , {source:65,target:62,value:5},{source:65,target:58,value:5},{source:65,target:61,value:5},{source:65,target:60,value:2}
  , {source:65,target:59,value:5},{source:65,target:57,value:1},{source:65,target:55,value:2},{source:66,target:64,value:3}
  , {source:66,target:58,value:3},{source:66,target:59,value:1},{source:66,target:62,value:2},{source:66,target:65,value:2}
  , {source:66,target:48,value:1},{source:66,target:63,value:1},{source:66,target:61,value:1},{source:66,target:60,value:1}
  , {source:67,target:57,value:3},{source:68,target:25,value:5},{source:68,target:11,value:1},{source:68,target:24,value:1}
  , {source:68,target:27,value:1},{source:68,target:48,value:1},{source:68,target:41,value:1},{source:69,target:25,value:6}
  , {source:69,target:68,value:6},{source:69,target:11,value:1},{source:69,target:24,value:1},{source:69,target:27,value:2}
  , {source:69,target:48,value:1},{source:69,target:41,value:1},{source:70,target:25,value:4},{source:70,target:69,value:4}
  , {source:70,target:68,value:4},{source:70,target:11,value:1},{source:70,target:24,value:1},{source:70,target:27,value:1}
  , {source:70,target:41,value:1},{source:70,target:58,value:1},{source:71,target:27,value:1},{source:71,target:69,value:2}
  , {source:71,target:68,value:2},{source:71,target:70,value:2},{source:71,target:11,value:1},{source:71,target:48,value:1}
  , {source:71,target:41,value:1},{source:71,target:25,value:1},{source:72,target:26,value:2},{source:72,target:27,value:1}
  , {source:72,target:11,value:1},{source:73,target:48,value:2},{source:74,target:48,value:2},{source:74,target:73,value:3}
  , {source:75,target:69,value:3},{source:75,target:68,value:3},{source:75,target:25,value:3},{source:75,target:48,value:1}
  , {source:75,target:41,value:1},{source:75,target:70,value:1},{source:75,target:71,value:1},{source:76,target:64,value:1}
  , {source:76,target:65,value:1},{source:76,target:66,value:1},{source:76,target:63,value:1},{source:76,target:62,value:1}
  , {source:76,target:48,value:1},{source:76,target:58,value:1}
  ]

-- =============================================================================
-- Constants
-- =============================================================================

-- Watercolour palette — diluted, desaturated versions of the old group colors
watercolourColors :: Array String
watercolourColors =
  [ "#b0ada8", "#d4917a", "#7aaf92", "#8e9bc4", "#c4a85c"
  , "#b08eb0", "#6abcbc", "#d4a878", "#9ab8a0", "#b09878", "#c48e9c"
  ]

svgW :: Number
svgW = 1200.0

svgH :: Number
svgH = 800.0

-- =============================================================================
-- Force configuration
-- =============================================================================

lesMisSetup =
  setup "lesmis"
    [ manyBody "charge" # withStrength (static (-304.0))
    , collide "collision" # withRadius (static 17.0) # withStrength (static 1.0)
    , link "links" # withDistance (static 30.0)
    , center "center"
    ]

-- =============================================================================
-- Plotter jitter
-- =============================================================================

jitterLine :: Int -> Number -> Number -> Number -> Number -> Number -> String
jitterLine seed wobble x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
      len = Number.sqrt (dx * dx + dy * dy)
  in if len < 1.0 then "M " <> s x1 <> " " <> s y1 <> " L " <> s x2 <> " " <> s y2
     else
       let px = negate dy / len
           py = dx / len
           g1 = Prng.gaussian seed
           g2 = Prng.gaussian g1.seed
           g3 = Prng.gaussian g2.seed
           m1x = x1 + dx * 0.25 + px * g1.value * wobble
           m1y = y1 + dy * 0.25 + py * g1.value * wobble
           m2x = x1 + dx * 0.5 + px * g2.value * wobble
           m2y = y1 + dy * 0.5 + py * g2.value * wobble
           m3x = x1 + dx * 0.75 + px * g3.value * wobble
           m3y = y1 + dy * 0.75 + py * g3.value * wobble
       in "M " <> s x1 <> " " <> s y1
         <> " L " <> s m1x <> " " <> s m1y
         <> " L " <> s m2x <> " " <> s m2y
         <> " L " <> s m3x <> " " <> s m3y
         <> " L " <> s x2 <> " " <> s y2

s :: Number -> String
s n = show (Int.toNumber (Int.floor (n * 10.0)) / 10.0)

-- =============================================================================
-- SVG helpers for raw elements (filters etc.)
-- =============================================================================

attr :: forall r i. String -> String -> HP.IProp r i
attr = HP.attr <<< HH.AttrName

-- =============================================================================
-- Halogen component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { nodes: []
      , links: lesMisLinks
      , handle: Nothing
      , blobs: precomputeBlobs
      , centerNodes: findCenterNodes lesMisLinks
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.id "lesmis-wrapper" ]
    [ SE.svg
        [ SA.viewBox ((-svgW) / 2.0) ((-svgH) / 2.0) svgW svgH
        , HP.style "width: 100%; height: 100%;"
        ]
        ( [ renderGradientDefs clusters ]
        <> [ SE.g [ HP.id "sim-internal", HP.style "display: none;" ] [] ]
        <> -- Watercolour blobs (behind everything)
          Array.mapMaybe (renderBlob state.blobs) clusters
        <> -- Edges
          map renderEdge (Array.range 0 (Array.length state.links - 1))
        <> -- Nodes (monochrome)
          map renderNode state.nodes
        <> -- Labels
          Array.mapMaybe renderLabel state.nodes
        )
    ]
  where
  clusters = computeClusters state.nodes

  renderGradientDefs _ =
    SE.defs [ HP.id "wc-defs" ] []

  renderBlob blobs cluster =
    case Array.find (\b -> b.group == cluster.group) blobs of
      Nothing -> Nothing
      Just blob ->
        let scale = max 40.0 cluster.radius / 60.0
            gradRef = "url(#wc-grad-" <> show cluster.group <> ")"
            xform = "translate(" <> show cluster.cx <> "," <> show cluster.cy <> ") scale(" <> show scale <> ")"
        in Just $ SE.g [ attr "transform" xform ]
          (map (\variant ->
            SE.path
              [ attr "d" (pathDataClosed variant)
              , attr "fill" gradRef
              , attr "fill-opacity" (show blob.opacity)
              , attr "stroke" "none"
              ]
          ) blob.variants)

  renderEdge idx = case Array.index state.links idx of
    Nothing -> SE.g [] []
    Just lnk ->
      let srcNode = Array.find (\n -> n.id == lnk.source) state.nodes
          tgtNode = Array.find (\n -> n.id == lnk.target) state.nodes
      in case srcNode, tgtNode of
        Just src, Just tgt ->
          SE.path
            [ attr "d" (jitterLine (idx * 7919 + 42) wobble src.x src.y tgt.x tgt.y)
            , attr "fill" "none"
            , SA.stroke (SA.Named "#1a1612")
            , SA.strokeWidth edgeWidth
            , SA.strokeOpacity edgeOpacity
            , attr "stroke-linecap" "round"
            ]
        _, _ -> SE.g [] []

  renderNode n =
    let deg = Array.length (Array.filter (\l -> l.source == n.id || l.target == n.id) state.links)
        r = (4.0 + Number.sqrt (Int.toNumber deg) * 2.0) * nodeScale
        isCenter = Array.elem n.id state.centerNodes
        colorIdx = n.group `mod` Array.length watercolourColors
        strokeColor = if isCenter
          then case Array.index watercolourColors colorIdx of
            Just c -> c
            Nothing -> "#1a1612"
          else "#1a1612"
        sw = if isCenter then 2.0 else 1.0
        so = if isCenter then 1.0 else 0.6
    in SE.circle
      [ SA.cx n.x
      , SA.cy n.y
      , SA.r r
      , SA.fill (SA.Named "#f5f0e8")
      , SA.stroke (SA.Named strokeColor)
      , SA.strokeWidth sw
      , SA.strokeOpacity so
      ]

  renderLabel n =
    let deg = Array.length (Array.filter (\l -> l.source == n.id || l.target == n.id) state.links)
        r = (4.0 + Number.sqrt (Int.toNumber deg) * 2.0) * nodeScale
    in if r > 9.0
      then Just $ SE.text
        [ SA.x n.x
        , SA.y (n.y - r - 3.0)
        , SA.textAnchor SA.AnchorMiddle
        , HP.style "font-family: Inter, sans-serif; font-size: 7px; fill: #1a1612; fill-opacity: 0.5;"
        ]
        [ HH.text n.name ]
      else Nothing

-- =============================================================================
-- Action handler
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect $ log "[LesMis] Creating simulation"

    let simLinks = map (\l -> { source: l.source, target: l.target }) lesMisLinks
        simNodes = Array.mapWithIndex (\i n ->
          { id: n.id, name: n.name, group: n.group
          , x: Int.toNumber (i `mod` 10 - 5) * 30.0
          , y: Int.toNumber (i / 10 - 4) * 30.0
          , vx: 0.0, vy: 0.0
          , fx: Nullable.null :: Nullable Number
          , fy: Nullable.null :: Nullable Number
          }) lesMisNodes
        config =
          { engine: D3
          , setup: lesMisSetup
          , nodes: simNodes
          , links: simLinks
          , container: "#sim-internal"
          , alphaMin: 0.001
          }

    result <- liftEffect $ runSimulation config

    halogenEmitter <- liftEffect $ toHalogenEmitter result.events
    void $ H.subscribe $ halogenEmitter <#> \event -> case event of
      Tick _ -> SimTick 0.0
      Completed -> SimDone
      Started -> SimTick 0.0
      Stopped -> SimDone

    H.modify_ _ { handle = Just result.handle }

    currentNodes <- liftEffect result.handle.getNodes
    H.modify_ _ { nodes = currentNodes }

    -- Inject radial gradient defs (SVG namespace requires innerHTML)
    let groups = nub $ map _.group lesMisNodes
        gradSvg = Array.foldl (\acc g ->
          let colorIdx = g `mod` Array.length watercolourColors
              color = case Array.index watercolourColors colorIdx of
                Just c -> c
                Nothing -> "#b0ada8"
          in acc <> "<radialGradient id=\"wc-grad-" <> show g <> "\">"
            <> "<stop offset=\"0%\" stop-color=\"" <> color <> "\" stop-opacity=\"1\"/>"
            <> "<stop offset=\"100%\" stop-color=\"" <> color <> "\" stop-opacity=\"0\"/>"
            <> "</radialGradient>"
        ) "" groups
    liftEffect $ Dom.setSvgContent "wc-defs" gradSvg

    -- Populate source code panels
    liftEffect $ Dom.setCode "code-forces"
      ( "-- force configuration\n"
      <> "lesMisSetup = setup \"lesmis\"\n"
      <> "  [ manyBody \"charge\" # withStrength (static (-304.0))\n"
      <> "  , collide \"collision\" # withRadius (static 17.0) # withStrength (static 1.0)\n"
      <> "  , link \"links\" # withDistance (static 30.0)\n"
      <> "  , center \"center\"\n"
      <> "  ]"
      )
    liftEffect $ Dom.setCode "code-blobs"
      ( "-- pre-compute watercolour blobs (once, at init)\n"
      <> "precomputeBlobs = map (\\g ->\n"
      <> "  let shape = (ellipseBlob { x: 0.0, y: 0.0 } 60.0 50.0 0.25 12 seed).polygon\n"
      <> "      wc    = watercolourBlob { layers: 20, depth: 5, displacement: 0.15 } seed shape\n"
      <> "  in { group: g, variants: wc.variants, opacity: 1.0 / 20.0 }\n"
      <> ") groups\n"
      <> "\n"
      <> "-- radial gradient defs (innerHTML for SVG namespace)\n"
      <> "<radialGradient id=\"wc-grad-{group}\">\n"
      <> "  <stop offset=\"0%\" stop-color=\"{color}\" stop-opacity=\"1\"/>\n"
      <> "  <stop offset=\"100%\" stop-color=\"{color}\" stop-opacity=\"0\"/>\n"
      <> "</radialGradient>"
      )
    liftEffect $ Dom.setCode "code-render"
      ( "-- render blob (each tick — geometry stable, only transform updates)\n"
      <> "renderBlob cluster =\n"
      <> "  let scale = max 40.0 cluster.radius / 60.0\n"
      <> "      xform = \"translate(\" <> show cx <> \",\" <> show cy <> \") scale(\" <> show scale <> \")\"\n"
      <> "  in SE.g [ attr \"transform\" xform ]\n"
      <> "    (map (\\variant ->\n"
      <> "      SE.path [ attr \"d\" (pathDataClosed variant)\n"
      <> "              , attr \"fill\" \"url(#wc-grad-{group})\"\n"
      <> "              , attr \"fill-opacity\" (show blob.opacity)\n"
      <> "              , attr \"stroke\" \"none\" ]\n"
      <> "    ) blob.variants)\n"
      <> "\n"
      <> "-- render node (monochrome, colored stroke for cluster centers)\n"
      <> "renderNode n =\n"
      <> "  let r = (4.0 + sqrt (toNumber deg) * 2.0) * nodeScale\n"
      <> "      strokeColor = if isCenter then clusterColor else \"#1a1612\"\n"
      <> "  in SE.circle [ SA.cx n.x, SA.cy n.y, SA.r r\n"
      <> "               , SA.fill (Named \"#f5f0e8\")\n"
      <> "               , SA.stroke (Named strokeColor) ]\n"
      <> "\n"
      <> "-- plotter-jittered edges\n"
      <> "renderEdge src tgt =\n"
      <> "  SE.path [ attr \"d\" (jitterLine seed wobble src.x src.y tgt.x tgt.y)\n"
      <> "          , SA.stroke (Named \"#1a1612\")\n"
      <> "          , SA.strokeWidth 0.6, SA.strokeOpacity 0.55 ]"
      )

  SimTick _ -> do
    state <- H.get
    case state.handle of
      Nothing -> pure unit
      Just handle -> do
        currentNodes <- liftEffect handle.getNodes
        H.modify_ _ { nodes = currentNodes }

  SimDone ->
    liftEffect $ log "[LesMis] Simulation converged"

-- =============================================================================
-- Entry point
-- =============================================================================

initLesMis :: Effect Unit
initLesMis = runHalogenAff do
  body <- awaitBody
  void $ runUI component unit body
