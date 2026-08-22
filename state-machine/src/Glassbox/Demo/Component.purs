-- | Glassbox.Demo.Component
-- |
-- | Steps 0 and 2 of Glassbox: one value, run and drawn — and drawn as the tree
-- | the user actually experiences.
-- |
-- | The acceptance test this component exists to satisfy, and to be able to
-- | fail, is that **the highlighted state is read from the running Mealy**.
-- | `State.step` is only ever written from `stepMealy`; nothing else in this
-- | module assigns it.
-- |
-- | The diagram lives in a container declared by the page, NOT in this
-- | component's tree. Halogen owns its own DOM and HATS owns its own DOM, and
-- | the two must not be the same node: an earlier version rendered into a
-- | Halogen-declared div and cleared it before each redraw, after which the
-- | component silently stopped updating at all.
module Glassbox.Demo.Component
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Machine.Mealy (Mealy, stepMealy)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import DataViz.Layout.StateMachine (StateMachine, StateMachineLayout, circularLayout, defaultConfig, layoutWithConfig)
import Effect.Aff.Class (class MonadAff)
import Glassbox.Codec.Mermaid (toMermaid)
import Glassbox.Demo.Loop (Beats, Cfg, Env, LoopEvent(..), Phase, defaultCfg, defaultEnv, loopMachine, userEvents)
import Glassbox.Demo.Render (Focus, diagramTree)
import Glassbox.Describe (EdgeExtra, annotate, defaultOptions, describe, machineEdges)
import Glassbox.Interpret (Input, Step, initialStep, toMealy)
import Glassbox.Layout.Tree (treeStrategy)
import Glassbox.Machine (Outcome(..), refusalText)
import Glassbox.Tree (EdgeClass(..), Induced, asymmetries, depthOf, edgeClassLabel, induce, pathFromRoot, returnCostOf)
import Halogen as H
import Halogen.Subscription as HS
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)

data LayoutMode = AsRing | AsTree

derive instance eqLayoutMode :: Eq LayoutMode

type State =
  { mealy :: Mealy (Input Env Cfg LoopEvent) (Step Phase LoopEvent)
  , step :: Step Phase LoopEvent          -- the ONLY source for the highlight
  , env :: Env
  , cfg :: Cfg
  , showRefusals :: Boolean
  , layoutMode :: LayoutMode
  , hover :: Maybe String
  -- HATS handlers are plain `Effect Unit`, so hovering has to be posted back
  -- into Halogen through a subscription rather than returned as an Action.
  , listener :: Maybe (HS.Listener Action)
  }

data Action
  = Initialize
  | Fire LoopEvent
  | Tick
  | SetCountIn Beats
  | ToggleConverter
  | ToggleRefusals
  | SetLayout LayoutMode
  | SetHover (Maybe String)

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { mealy: toMealy loopMachine
      , step: initialStep loopMachine PressStop
      , env: defaultEnv
      , cfg: defaultCfg
      , showRefusals: true
      , layoutMode: AsTree
      , hover: Nothing
      , listener: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    void (H.subscribe emitter)
    H.modify_ \s -> s { listener = Just listener }
    redraw

  Fire event -> do
    H.modify_ (advance event)
    redraw

  Tick -> do
    state <- H.get
    let ticked = state { env = state.env { nowBeats = state.env.nowBeats + 1.0 } }
    H.put (if dueNow ticked then advance Elapsed ticked else ticked)
    redraw

  SetCountIn n -> do
    H.modify_ \s -> s { cfg = s.cfg { countInBeats = n } }
    redraw

  ToggleConverter -> do
    H.modify_ \s -> s
      { env = s.env
          { converterHeldBy = case s.env.converterHeldBy of
              Just _ -> Nothing
              Nothing -> Just 3
          }
      }
    redraw

  ToggleRefusals -> do
    H.modify_ \s -> s { showRefusals = not s.showRefusals }
    redraw

  SetLayout mode -> do
    H.modify_ \s -> s { layoutMode = mode }
    redraw

  SetHover target -> do
    current <- H.gets _.hover
    -- Redrawing the whole diagram on every mouse move would be silly; only a
    -- change of target is worth a repaint.
    when (current /= target) do
      H.modify_ \s -> s { hover = target }
      redraw

-- | Feed one event through the Mealy and take the machine's word for the result.
advance :: LoopEvent -> State -> State
advance event state =
  let
    input = { env: state.env, cfg: state.cfg, event }
    Tuple nextMealy step = stepMealy state.mealy input
    entered =
      if step.current == step.from then state.env.enteredAt else state.env.nowBeats
  in
    state
      { mealy = nextMealy
      , step = step
      , env = state.env { enteredAt = entered }
      }

dueNow :: State -> Boolean
dueNow state = case loopMachine.pending state.env state.cfg state.step.current of
  Just { at } -> state.env.nowBeats >= at
  Nothing -> false

-- | Clear before redrawing.
-- |
-- | The edge count genuinely changes — a different count-in reshapes the
-- | machine, and refusals can be hidden — and HATS joins by position, so a
-- | shorter list would leave the tail of the previous drawing on screen wearing
-- | the wrong attributes.
redraw :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
redraw = do
  state <- H.get
  let
    callbacks =
      { hover: case state.listener of
          Just listener -> \target -> HS.notify listener (SetHover target)
          Nothing -> \_ -> pure unit
      }
  H.liftEffect do
    clearContainer "#glassbox-diagram"
    void $ rerender "#glassbox-diagram"
      ( diagramTree callbacks
          (loopMachine.stateId state.step.current)
          (focusOf state)
          (laidOut state)
      )

-- | What to keep lit when the reader is pointing at a state.
-- |
-- | Two things, and the first is the one worth having: **the path home**, from
-- | the induced tree, which answers the question a user actually has — *how do I
-- | get here* — and which no amount of edge routing would have made legible in a
-- | crowded picture. The second is the state's own incident edges: what you can
-- | do once you arrive, and what else leads here.
focusOf :: State -> Maybe Focus
focusOf state = state.hover <#> \target ->
  let
    tree = induced state
    route = pathFromRoot tree target
    routeEdges = Array.zip route (Array.drop 1 route)
    incident = Array.filter (\e -> e.from == target || e.to == target)
      (annotated state).transitions
  in
    { states: Array.nub (route <> [ target ] <> map _.from incident <> map _.to incident)
    , edges: Array.nub (routeEdges <> map (\e -> Tuple e.from e.to) incident)
    }

-- ---------------------------------------------------------------------------
-- The three readings of the machine, all from one value
-- ---------------------------------------------------------------------------

described :: State -> StateMachine Unit EdgeExtra
described state =
  describe (defaultOptions { showRefusals = state.showRefusals })
    state.env
    state.cfg
    loopMachine

induced :: State -> Induced
induced state =
  let base = described state
  in induce (loopMachine.stateId loopMachine.initial)
       (map _.id base.states)
       (machineEdges base)

annotated :: State -> StateMachine Unit EdgeExtra
annotated state = annotate (induced state) (described state)

laidOut :: State -> StateMachineLayout Unit EdgeExtra
laidOut state = case state.layoutMode of
  AsRing -> layoutWithConfig defaultConfig circularLayout (annotated state)
  AsTree ->
    -- Nearly straight links: a ring needs its chords bowed apart, a tidy tree
    -- does not, and the bow is what was throwing the labels on top of each
    -- other.
    layoutWithConfig (defaultConfig { edgeCurvature = 0.04 })
      (treeStrategy (induced state))
      (annotated state)

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ statusBlock state
    , transportBlock
    , settingsBlock state
    , shapeBlock state
    , mermaidBlock state
    ]

statusBlock :: forall m. State -> H.ComponentHTML Action () m
statusBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "State" ]
    , HH.p [ HP.class_ (H.ClassName "current") ]
        [ HH.text (loopMachine.stateLabel state.step.current) ]
    , case state.step.outcome of
        Refused refusal ->
          HH.p [ HP.class_ (H.ClassName "refusal") ]
            [ HH.strong_ [ HH.text "refused — " ], HH.text (refusalText refusal) ]
        Stay -> HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text "nothing to do here" ]
        Move _ ->
          HH.p [ HP.class_ (H.ClassName "quiet") ]
            [ HH.text (loopMachine.eventLabel state.step.event) ]
    , pendingLine state
    ]

pendingLine :: forall m. State -> H.ComponentHTML Action () m
pendingLine state =
  case loopMachine.pending state.env state.cfg state.step.current of
    Just { at } ->
      HH.p [ HP.class_ (H.ClassName "pending") ]
        [ HH.text ("starts in " <> beats (at - state.env.nowBeats) <> " beats") ]
    Nothing -> HH.text ""

transportBlock :: forall m. H.ComponentHTML Action () m
transportBlock =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Footswitch" ]
    , HH.div [ HP.class_ (H.ClassName "buttons") ]
        (map button userEvents <> [ tickButton ])
    ]
  where
  button event =
    HH.button [ HE.onClick \_ -> Fire event ]
      [ HH.text (loopMachine.eventLabel event) ]

  tickButton =
    HH.button [ HE.onClick \_ -> Tick, HP.class_ (H.ClassName "tick") ]
      [ HH.text "tick +1 beat" ]

settingsBlock :: forall m. State -> H.ComponentHTML Action () m
settingsBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Configuration" ]
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "Count-in reshapes the machine — watch Armed strand itself." ]
    , HH.div [ HP.class_ (H.ClassName "buttons") ]
        (map countInButton [ 0.0, 2.0, 4.0 ])
    , HH.div [ HP.class_ (H.ClassName "buttons") ]
        [ layoutButton AsTree "as the user's tree"
        , layoutButton AsRing "as a ring"
        ]
    , HH.label_
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked (isJust state.env.converterHeldBy)
            , HE.onChange \_ -> ToggleConverter
            ]
        , HH.text " loop 3 holds the converter"
        ]
    , HH.label_
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.showRefusals
            , HE.onChange \_ -> ToggleRefusals
            ]
        , HH.text " draw refusals"
        ]
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text ("now: beat " <> beats state.env.nowBeats) ]
    ]
  where
  countInButton n =
    HH.button
      [ HE.onClick \_ -> SetCountIn n
      , HP.class_ (H.ClassName (if state.cfg.countInBeats == n then "on" else ""))
      ]
      [ HH.text (beats n <> " beat count-in") ]

  layoutButton mode label =
    HH.button
      [ HE.onClick \_ -> SetLayout mode
      , HP.class_ (H.ClassName (if state.layoutMode == mode then "on" else ""))
      ]
      [ HH.text label ]

-- | What the induced tree says about the machine.
-- |
-- | Not decoration: this is the first of the lints, and every number in it comes
-- | from the same value the diagram is drawn from.
shapeBlock :: forall m. State -> H.ComponentHTML Action () m
shapeBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Shape" ]
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text (reading) ]
    , HH.ul [ HP.class_ (H.ClassName "classes") ] (map classRow presentClasses)
    , unreachableLine
    , asymmetryLine
    ]
  where
  tree = induced state
  here = loopMachine.stateId state.step.current

  -- Hovering asks about a state you are not in, which is the more useful
  -- question: the panel follows the pointer when there is one.
  reading = case state.hover of
    Just target -> labelFor target <> ": " <> costs target
    Nothing -> "here: " <> costs here

  labelFor id = case Array.find (\p -> loopMachine.stateId p == id) loopMachine.states of
    Just phase -> loopMachine.stateLabel phase
    Nothing -> id

  costs node
    | node == tree.root = "home"
    | otherwise =
        maybe "unreachable" show (depthOf tree node) <> " presses in, "
          <> maybe "no way back" show (returnCostOf tree node) <> " back out"

  counts =
    Map.toUnfoldable (Map.fromFoldableWith (+) (map (\c -> Tuple c 1) allRoles))
      :: Array (Tuple EdgeClass Int)

  allRoles = Array.mapMaybe (\e -> e.extra.role) (annotated state).transitions

  presentClasses = counts

  classRow (Tuple cls n) =
    HH.li [ HP.class_ (H.ClassName ("cls cls--" <> edgeClassLabel cls)) ]
      [ HH.span [ HP.class_ (H.ClassName "swatch") ] []
      , HH.text (edgeClassLabel cls <> " × " <> show n)
      , HH.span [ HP.class_ (H.ClassName "gloss") ] [ HH.text (gloss cls) ]
      ]

  gloss = case _ of
    TreeEdge -> "how you first arrive"
    BackEdge -> "the way out"
    ForwardEdge -> "a shortcut past a level"
    CrossEdge -> "a jump into another branch"
    SelfEdge -> "goes nowhere"
    FromUnreachable -> "leaves a state you cannot reach"

  unreachableLine = case tree.unreachable of
    [] -> HH.text ""
    states ->
      HH.p [ HP.class_ (H.ClassName "refusal") ]
        [ HH.strong_ [ HH.text "unreachable — " ]
        , HH.text (Array.intercalate ", " states)
        ]

  asymmetryLine = case Array.head (asymmetries tree) of
    Just worst ->
      HH.p [ HP.class_ (H.ClassName "pending") ]
        [ HH.text
            ( "worst return: " <> worst.state <> ", in by " <> show worst.inCost
                <> ", out by "
                <> show worst.outCost
            )
        ]
    Nothing ->
      HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "no state costs more to leave than to reach" ]

mermaidBlock :: forall m. State -> H.ComponentHTML Action () m
mermaidBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Mermaid" ]
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "The same description, emitted for an independent renderer." ]
    , HH.pre_ [ HH.code_ [ HH.text (toMermaid (annotated state)) ] ]
    ]

beats :: Number -> String
beats = toStringWith (fixed 0)
