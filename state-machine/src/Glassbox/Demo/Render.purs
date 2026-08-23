-- | Glassbox.Demo.Render
-- |
-- | HATS rendering of a laid-out machine.
-- |
-- | The live state arrives here as a separate `currentId` argument rather than
-- | baked into the description. That is deliberate: the *structure* comes from
-- | the machine and the *highlight* comes from the running Mealy, so neither can
-- | quietly supply the other's answer.
module Glassbox.Demo.Render
  ( Callbacks
  , Focus
  , diagramTree
  ) where

import Prelude

import Data.Array (elem)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import DataViz.Layout.StateMachine (LayoutState, LayoutTransition, StateMachineLayout, arrowheadPathD, initialArrowPathD, transitionPathD)
import Glassbox.Draw (EdgeExtra, StateExtra)
import Glassbox.Edges (EdgeKind(..))
import Data.Graph.InducedTree (EdgeClass(..))
import Hylograph.HATS (Tree, onMouseEnter, onMouseLeave, staticNum, staticStr, withBehaviors)
import Hylograph.HATS (elem) as HATS
import Hylograph.Internal.Element.Types (ElementType(..))

-- Swiss-ish palette: one accent, everything else greyscale.
accent :: String
accent = "#c8102e"

ink :: String
ink = "#1a1a1a"

muted :: String
muted = "#8a8a8a"

-- Two more hues, only for the two edge roles that carry a verdict: a shortcut
-- past a level, and a jump into another branch. Everything else stays ink.
shortcut :: String
shortcut = "#1f6feb"

jump :: String
jump = "#b8860b"

-- | What the reader is currently isolating.
-- |
-- | Crowding is a problem of *simultaneous* legibility. If a reader can pull one
-- | state's story out of the tangle on hover, the static drawing only has to be
-- | navigable rather than perfect — which is a far cheaper thing to achieve than
-- | a layout with no collisions in it.
type Focus =
  { states :: Array String
  , edges :: Array (Tuple String String)
  }

type Callbacks =
  { hover :: Maybe String -> Effect Unit
  }

diagramTree :: Callbacks -> String -> Maybe Focus -> StateMachineLayout StateExtra EdgeExtra -> Tree
diagramTree callbacks currentId focus layout =
  HATS.elem SVG
    [ staticStr "viewBox"
        ( show layout.originX <> " " <> show layout.originY <> " "
            <> show layout.width
            <> " "
            <> show layout.height
        )
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    , staticStr "width" "100%"
    ]
    ( [ initialArrow ]
        <> map (edgeGroup focus) layout.transitions
        <> map (stateGroup callbacks currentId focus) layout.states
    )
  where
  initialArrow =
    HATS.elem Path
      [ staticStr "d" (initialArrowPathD layout.initialArrow 8.0)
      , staticStr "fill" ink
      , staticStr "stroke" ink
      , staticStr "stroke-width" "1.5"
      ]
      []

edgeGroup :: Maybe Focus -> LayoutTransition EdgeExtra -> Tree
edgeGroup focus t =
  HATS.elem Group
    [ staticStr "opacity" (fade focus inFocus) ]
    (edgeElements t)
  where
  inFocus = case focus of
    Nothing -> true
    Just f -> elem (Tuple t.transition.from t.transition.to) f.edges

edgeElements :: LayoutTransition EdgeExtra -> Array Tree
edgeElements { transition, path } =
  [ HATS.elem Path
      [ staticStr "d" (transitionPathD path)
      , staticStr "fill" "none"
      , staticStr "stroke" stroke
      , staticStr "stroke-width" width
      , staticStr "stroke-dasharray" dash
      , staticStr "stroke-opacity" opacity
      ]
      tooltip
  , HATS.elem Path
      [ staticStr "d" (arrowheadPathD path.endX path.endY path.angle 7.0)
      , staticStr "fill" stroke
      , staticStr "fill-opacity" opacity
      ]
      []
  , HATS.elem Text
      [ staticNum "x" path.labelX
      , staticNum "y" path.labelY
      , staticStr "text-anchor" "middle"
      , staticStr "dominant-baseline" "middle"
      , staticStr "font-size" "10"
      , staticStr "font-family" "system-ui, -apple-system, sans-serif"
      , staticStr "fill" stroke
      , staticStr "textContent" label
      ]
      []
  ]
  where
  kind = transition.extra.kind
  role = transition.extra.role

  -- How an edge came to exist wins where it says something the role cannot: a
  -- deadline and a refusal are facts about the machine, not about navigation.
  -- Otherwise the edge is styled by what it means to someone navigating.
  stroke = case kind, role of
    OnDeadline, _ -> accent
    OnRefusal, _ -> muted
    _, Just ForwardEdge -> shortcut
    _, Just CrossEdge -> jump
    _, Just SelfEdge -> muted
    _, Just FromUnreachable -> pale
    _, _ -> ink

  width = case kind, role of
    OnDeadline, _ -> "2"
    _, Just TreeEdge -> "2.2"
    _, Just BackEdge -> "1"
    _ , _ -> "1.4"

  dash = case kind, role of
    OnDeadline, _ -> "5,3"
    OnRefusal, _ -> "2,3"
    _, Just ForwardEdge -> "7,3"
    _, Just CrossEdge -> "1,4"
    _, _ -> ""

  opacity = case kind, role of
    OnRefusal, _ -> "0.55"
    _, Just BackEdge -> "0.45"
    _, Just FromUnreachable -> "0.3"
    _, _ -> "1"

  -- The reason stays off the canvas: refusal reasons are sentences, and a
  -- sentence used as an edge label overflows the diagram it is meant to
  -- explain. It rides in a `<title>` instead, and the status pane says it in
  -- full when a refusal actually happens.
  label = case kind of
    OnDeadline -> "after " <> transition.label
    _ -> transition.label

  -- Always emitted, for the same positional-join reason as the halo below.
  tooltip =
    [ HATS.elem Title
        [ staticStr "textContent" (fromMaybe "" transition.extra.reason) ]
        []
    ]

-- | Always three elements per state, never two.
-- |
-- | HATS joins by position, so emitting the halo only for the live state
-- | changes the length of the child list and shifts every element after it —
-- | which paints one state's attributes onto its neighbour and makes the
-- | diagram disagree with the machine for reasons that have nothing to do with
-- | the machine. The halo is always present and merely invisible when idle.
stateGroup :: Callbacks -> String -> Maybe Focus -> LayoutState StateExtra -> Tree
stateGroup callbacks currentId focus placed =
  withBehaviors
    [ onMouseEnter (callbacks.hover (Just placed.state.id))
    , onMouseLeave (callbacks.hover Nothing)
    ]
    ( HATS.elem Group
        [ staticStr "opacity" (fade focus (stateInFocus placed.state.id))
        , staticStr "cursor" "pointer"
        ]
        (stateElements currentId placed)
    )
  where
  stateInFocus id = case focus of
    Just f -> elem id f.states
    Nothing -> true

-- | Five elements per state since commands landed, and the last two are always
-- | present even when they have nothing to say — see the note above about HATS
-- | joining by position. A state that runs nothing gets an empty label rather
-- | than no label.
stateElements :: String -> LayoutState StateExtra -> Array Tree
stateElements currentId { state, position } =
      [ halo
      , HATS.elem Path
          [ staticStr "d" (ellipseD position.cx position.cy position.rx position.ry)
          , staticStr "fill" (if isCurrent then accent else "#ffffff")
          , staticStr "stroke" (if isCurrent then accent else ink)
          , staticStr "stroke-width" (if isCurrent then "2" else "1.5")
          ]
          []
      , HATS.elem Text
          [ staticNum "x" position.cx
          , staticNum "y" (position.cy + 1.0)
          , staticStr "text-anchor" "middle"
          , staticStr "dominant-baseline" "middle"
          , staticStr "font-size" "12"
          , staticStr "font-weight" (if isCurrent then "600" else "500")
          , staticStr "font-family" "system-ui, -apple-system, sans-serif"
          , staticStr "fill" (if isCurrent then "#ffffff" else ink)
          , staticStr "textContent" state.label
          ]
          []
      , caption 11.0 (prefixed "+" state.extra.entry)
      , caption 20.0 (prefixed "\x2212" state.extra.exit)
      ]
  where
  isCurrent = state.id == currentId

  -- `+` on the way in, `−` on the way out, on two lines rather than one.
  -- One line was legible in isolation and collided with the neighbouring
  -- state's caption the moment two nodes sat close together, which in a
  -- machine of this size is most of them. The panel beside the drawing carries
  -- the full sentence; this is only the reminder that a command lives HERE, on
  -- the state, and not on any of the arrows.
  prefixed mark = joinWith " " <<< map (\c -> mark <> c)

  caption dy text =
    HATS.elem Text
      [ staticNum "x" position.cx
      , staticNum "y" (position.cy + position.ry + dy)
      , staticStr "text-anchor" "middle"
      , staticStr "font-size" "8.5"
      , staticStr "font-family" "ui-monospace, SFMono-Regular, Menlo, monospace"
      , staticStr "fill" "#9a9a9a"
      , staticStr "textContent" text
      ]
      []

  halo =
    HATS.elem Path
      [ staticStr "d" (ellipseD position.cx position.cy (position.rx + 6.0) (position.ry + 6.0))
      , staticStr "fill" "none"
      , staticStr "stroke" accent
      , staticStr "stroke-width" "1"
      , staticStr "stroke-opacity" (if isCurrent then "0.35" else "0")
      ]
      []

-- | Everything outside the focus recedes almost to nothing. Hard dimming is the
-- | point: a gentle fade leaves the tangle competing for attention, which is the
-- | problem being solved.
fade :: Maybe Focus -> Boolean -> String
fade focus inFocus = case focus of
  Nothing -> "1"
  Just _ -> if inFocus then "1" else "0.06"

pale :: String
pale = "#cfcfcf"

-- | An SVG ellipse as a path, because `ElementType` has no `Ellipse`
-- | constructor. Two half-arcs, closed.
ellipseD :: Number -> Number -> Number -> Number -> String
ellipseD cx cy rx ry =
  "M " <> show (cx - rx) <> " " <> show cy
    <> " a " <> show rx <> " " <> show ry <> " 0 1 0 " <> show (2.0 * rx) <> " 0"
    <> " a " <> show rx <> " " <> show ry <> " 0 1 0 " <> show (negate (2.0 * rx)) <> " 0"
    <> " Z"
