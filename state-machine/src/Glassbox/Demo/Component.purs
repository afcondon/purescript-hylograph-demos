-- | Glassbox.Demo.Component
-- |
-- | A machine inspector: it loads an artifact and shows you what it does.
-- |
-- | The acceptance test this component exists to satisfy, and to be able to
-- | fail, is that **it knows nothing about any particular machine**. There is no
-- | `Phase`, no `LoopEvent`, no count-in and no converter anywhere in this file.
-- | Every button, toggle and field below is generated from what the artifact
-- | declares about itself, which is why the same component runs a guitar looper
-- | and a car radio without being recompiled. If a machine ever needs a line
-- | here, the thesis has failed.
-- |
-- | The current state is a plain `StateId` held in the component's own state —
-- | comparable, printable, serialisable. An earlier version stored a `Mealy`,
-- | which meant the same phase lived in two places and one of them was a
-- | closure.
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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import DataViz.Layout.StateMachine (StateMachine, StateMachineLayout, circularLayout, defaultConfig, layoutWithConfig, treeStrategy)
import Effect.Aff.Class (class MonadAff)
import Glassbox.Codec.JSON (parseSpec, printSpecPretty)
import Glassbox.Export.StateDiagram (toStateDiagram)
import Glassbox.Demo.Fetch (fetchText)
import Glassbox.Demo.Render (Focus, diagramTree)
import Glassbox.Describe (EdgeExtra, annotate, defaultOptions, describe, machineEdges)

import Glassbox.Run (World, dueAt, lookupConfig, lookupFact, setConfig, setFact, step, worldFrom)
import Glassbox.Spec
  ( ConfigId
  , EventId
  , FactId
  , Outcome(..)
  , Spec
  , StateId(..)
  , Value(..)
  , labelOfEvent
  , labelOfState
  , textOfRefusal
  , userEvents
  )
import Data.Graph.Algorithms (mkSimpleGraph)
import Data.Graph.InducedTree (Induced, induce, pathFromRoot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)

-- | The artifacts on offer. Adding one here is a link in a menu, not a feature:
-- | the machine itself is entirely in the file.
catalogue :: Array { file :: String, name :: String }
catalogue =
  [ { file: "machines/loop.json", name: "Guitar looper" }
  , { file: "machines/car-radio.json", name: "Car radio" }
  , { file: "machines/elevator.json", name: "Elevator" }
  , { file: "machines/washing-machine.json", name: "Washing machine" }
  , { file: "machines/heating.json", name: "Heating" }
  ]

data LayoutMode = AsRing | AsTree

derive instance eqLayoutMode :: Eq LayoutMode

-- | The machine has more than one textual reading, and neither is the source
-- | of the drawing — all three come off the same decoded value.
data TextView
  = AsArtifact  -- ^ the machine as the decoder understood it, re-encoded
  | AsDiagramText -- ^ the same drawing as text, in a notation GitHub renders

derive instance eqTextView :: Eq TextView

-- | `Nothing` for `loaded` means the artifact has not arrived, or did not
-- | survive the boundary. A host with no valid machine shows why and offers
-- | nothing else — it does not carry on with a default.
type State =
  { loaded :: Maybe Loaded
  , error :: Maybe String
  , source :: String
  , showRefusals :: Boolean
  , layoutMode :: LayoutMode
  , hover :: Maybe String
  , textView :: TextView
  , textOpen :: Boolean
  -- HATS handlers are plain `Effect Unit`, so hovering has to be posted back
  -- into Halogen through a subscription rather than returned as an Action.
  , listener :: Maybe (HS.Listener Action)
  }

type Loaded =
  { spec :: Spec
  , world :: World
  , current :: StateId
  , last :: Maybe { event :: EventId, outcome :: Outcome }
  , now :: Number
  , enteredAt :: Number
  }

data Action
  = Initialize
  | Load String
  | Fire EventId
  | Tick
  | SetFact FactId Boolean
  | SetConfigNumber ConfigId String
  | SetConfigBool ConfigId Boolean
  | ToggleRefusals
  | SetLayout LayoutMode
  | SetHover (Maybe String)
  | SetTextView TextView
  | ToggleText

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { loaded: Nothing
      , error: Nothing
      , source: fromMaybe "" (map _.file (Array.head catalogue))
      , showRefusals: true
      , layoutMode: AsTree
      , hover: Nothing
      , textView: AsArtifact
      , textOpen: false
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
    source <- H.gets _.source
    handleAction (Load source)

  Load file -> do
    text <- H.liftAff (fetchText file)
    -- The boundary is checked, not trusted: a machine that does not decode does
    -- not run, and the reason is shown rather than swallowed.
    case parseSpec text of
      Left err -> H.modify_ \s -> s { loaded = Nothing, error = Just err, source = file }
      Right spec -> H.modify_ \s -> s
        { source = file
        , error = Nothing
        , hover = Nothing
        , loaded = Just
            { spec
            , world: worldFrom spec
            , current: spec.initial
            , last: Nothing
            , now: 0.0
            , enteredAt: 0.0
            }
        }
    redraw

  Fire event -> do
    H.modify_ (overLoaded (advance event))
    redraw

  Tick -> do
    H.modify_ (overLoaded tick)
    redraw

  SetFact key on -> do
    H.modify_ (overLoaded \l -> l { world = setFact key (VBoolean on) l.world })
    redraw

  SetConfigBool key on -> do
    H.modify_ (overLoaded \l -> l { world = setConfig key (VBoolean on) l.world })
    redraw

  SetConfigNumber key raw -> do
    case Number.fromString raw of
      Nothing -> pure unit
      Just n -> H.modify_ (overLoaded \l -> l { world = setConfig key (VNumber n) l.world })
    redraw

  ToggleRefusals -> do
    H.modify_ \s -> s { showRefusals = not s.showRefusals }
    redraw

  SetLayout mode -> do
    H.modify_ \s -> s { layoutMode = mode }
    redraw

  SetTextView view -> H.modify_ \s -> s { textView = view, textOpen = true }

  ToggleText -> H.modify_ \s -> s { textOpen = not s.textOpen }

  SetHover target -> do
    current <- H.gets _.hover
    -- Redrawing the whole diagram on every mouse move would be silly; only a
    -- change of target is worth a repaint.
    when (current /= target) do
      H.modify_ \s -> s { hover = target }
      redraw

overLoaded :: (Loaded -> Loaded) -> State -> State
overLoaded f state = state { loaded = map f state.loaded }

-- | Feed one event through the machine and take its word for the result.
advance :: EventId -> Loaded -> Loaded
advance event l =
  let
    Tuple next record = step l.spec l.world l.current event
  in
    l
      { current = next
      , last = Just { event, outcome: record.outcome }
      , enteredAt = if next == l.current then l.enteredAt else l.now
      }

-- | Advance the clock, and deliver the deadline's event if it has come due.
-- |
-- | The runtime owns deadline delivery, which is what a declared `pending`
-- | buys: there is no timer to leave running when the state changes.
tick :: Loaded -> Loaded
tick l =
  let
    ticked = l { now = l.now + 1.0 }
  in
    case dueAt ticked.spec ticked.world ticked.current ticked.enteredAt of
      Just { fires, at } | ticked.now >= at -> advance fires ticked
      _ -> ticked

-- ---------------------------------------------------------------------------
-- The readings of the machine, all from one value
-- ---------------------------------------------------------------------------

described :: State -> Loaded -> StateMachine Unit EdgeExtra
described state l =
  describe (defaultOptions { showRefusals = state.showRefusals }) l.world l.spec

induced :: State -> Loaded -> Induced String
induced state l =
  let
    base = described state l
    StateId root = l.spec.initial
  in
    induce root (mkSimpleGraph (map _.id base.states) (machineEdges base))

annotated :: State -> Loaded -> StateMachine Unit EdgeExtra
annotated state l = annotate (induced state l) (described state l)

laidOut :: State -> Loaded -> StateMachineLayout Unit EdgeExtra
laidOut state l = case state.layoutMode of
  AsRing -> layoutWithConfig defaultConfig circularLayout (annotated state l)
  AsTree ->
    -- Nearly straight links: a ring needs its chords bowed apart, a tidy tree
    -- does not, and the bow is what was throwing the labels on top of each
    -- other.
    layoutWithConfig (defaultConfig { edgeCurvature = 0.04 })
      (treeStrategy (induced state l))
      (annotated state l)

-- | What to keep lit when the reader is pointing at a state.
-- |
-- | Two things, and the first is the one worth having: **the path home**, from
-- | the induced tree, which answers the question a user actually has — *how do I
-- | get here* — and which no amount of edge routing would have made legible in a
-- | crowded picture.
focusOf :: State -> Loaded -> Maybe Focus
focusOf state l = state.hover <#> \target ->
  let
    route = pathFromRoot (induced state l) target
    routeEdges = Array.zip route (Array.drop 1 route)
    incident = Array.filter (\e -> e.from == target || e.to == target)
      (annotated state l).transitions
  in
    { states: Array.nub (route <> [ target ] <> map _.from incident <> map _.to incident)
    , edges: Array.nub (routeEdges <> map (\e -> Tuple e.from e.to) incident)
    }

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
    -- The edge count genuinely changes — a different config reshapes the
    -- machine, and refusals can be hidden — and HATS joins by position, so a
    -- shorter list would leave the tail of the previous drawing on screen
    -- wearing the wrong attributes.
    clearContainer "#glassbox-diagram"
    case state.loaded of
      Nothing -> pure unit
      Just l -> void $ rerender "#glassbox-diagram"
        ( diagramTree callbacks
            (case l.current of StateId s -> s)
            (focusOf state l)
            (laidOut state l)
        )

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    ( [ pickerBlock state ]
        <> case state.error, state.loaded of
          Just err, _ -> [ errorBlock err ]
          _, Nothing -> [ HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text "loading…" ] ]
          _, Just l ->
            [ statusBlock state l
            , eventsBlock l
            , factsBlock l
            , configBlock l
            , shapeBlock state
            , textBlock state l
            ]
    )

errorBlock :: forall m. String -> H.ComponentHTML Action () m
errorBlock err =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "This artifact did not load" ]
    , HH.p [ HP.class_ (H.ClassName "refusal") ] [ HH.text err ]
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "A machine that fails the boundary does not run." ]
    ]

pickerBlock :: forall m. State -> H.ComponentHTML Action () m
pickerBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Machine" ]
    , HH.div [ HP.class_ (H.ClassName "buttons") ] (map button catalogue)
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "Both are JSON files fetched at runtime. Nothing here was compiled to know about either." ]
    ]
  where
  button entry =
    HH.button
      [ HE.onClick \_ -> Load entry.file
      , HP.class_ (H.ClassName (if entry.file == state.source then "on" else ""))
      ]
      [ HH.text entry.name ]

statusBlock :: forall m. State -> Loaded -> H.ComponentHTML Action () m
statusBlock _ l =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text l.spec.title ]
    , HH.p [ HP.class_ (H.ClassName "current") ]
        [ HH.text (labelOfState l.spec l.current) ]
    , case l.last of
        Nothing -> HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text "nothing pressed yet" ]
        Just { event, outcome } -> case outcome of
          Refuse rid ->
            HH.p [ HP.class_ (H.ClassName "refusal") ]
              [ HH.strong_ [ HH.text "refused — " ], HH.text (textOfRefusal l.spec rid) ]
          Stay ->
            HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text "nothing to do here" ]
          Move _ ->
            HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text (labelOfEvent l.spec event) ]
    , pendingLine l
    ]

pendingLine :: forall m. Loaded -> H.ComponentHTML Action () m
pendingLine l = case dueAt l.spec l.world l.current l.enteredAt of
  Just { fires, at } ->
    HH.p [ HP.class_ (H.ClassName "pending") ]
      [ HH.text (labelOfEvent l.spec fires <> " in " <> toStringWith (fixed 0) (at - l.now)) ]
  Nothing -> HH.text ""

eventsBlock :: forall m. Loaded -> H.ComponentHTML Action () m
eventsBlock l =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Events" ]
    , HH.div [ HP.class_ (H.ClassName "buttons") ]
        (map button (userEvents l.spec) <> [ tickButton ])
    , HH.p [ HP.class_ (H.ClassName "quiet") ]
        [ HH.text "Runtime events are not offered — only the machine's clock delivers those." ]
    ]
  where
  button event =
    HH.button [ HE.onClick \_ -> Fire event ] [ HH.text (labelOfEvent l.spec event) ]

  tickButton =
    HH.button [ HE.onClick \_ -> Tick, HP.class_ (H.ClassName "tick") ]
      [ HH.text "tick +1" ]

factsBlock :: forall m. Loaded -> H.ComponentHTML Action () m
factsBlock l
  | Array.null l.spec.facts = HH.text ""
  | otherwise =
      HH.div [ HP.class_ (H.ClassName "block") ]
        [ HH.h2_ [ HH.text "The world" ]
        , HH.p [ HP.class_ (H.ClassName "quiet") ]
            [ HH.text "Facts the host reports. They change what an event means, not what the machine is." ]
        , HH.div_ (map row l.spec.facts)
        ]
      where
      row decl =
        HH.label [ HP.class_ (H.ClassName "toggle") ]
          [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked (factIsSet l decl.id)
              , HE.onChecked (SetFact decl.id)
              ]
          , HH.text decl.label
          ]

factIsSet :: Loaded -> FactId -> Boolean
factIsSet l key = case lookupFact l.world key of
  Just (VBoolean b) -> b
  _ -> false

configBlock :: forall m. Loaded -> H.ComponentHTML Action () m
configBlock l
  | Array.null l.spec.config = HH.text ""
  | otherwise =
      HH.div [ HP.class_ (H.ClassName "block") ]
        [ HH.h2_ [ HH.text "Configuration" ]
        , HH.p [ HP.class_ (H.ClassName "quiet") ]
            [ HH.text "Config reshapes the machine — watch a state strand itself as you change one." ]
        , HH.div_ (map row l.spec.config)
        ]
      where
      row decl = case configValue l decl.id decl.default of
        VBoolean b ->
          HH.label [ HP.class_ (H.ClassName "toggle") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked b
                , HE.onChecked (SetConfigBool decl.id)
                ]
            , HH.text decl.label
            ]
        VNumber n ->
          HH.label [ HP.class_ (H.ClassName "field") ]
            [ HH.text decl.label
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.value (toStringWith (fixed 0) n)
                , HE.onValueInput (SetConfigNumber decl.id)
                ]
            ]
        VString s ->
          HH.label [ HP.class_ (H.ClassName "field") ]
            [ HH.text decl.label, HH.span_ [ HH.text s ] ]

configValue :: Loaded -> ConfigId -> Value -> Value
configValue l key fallback =
  fromMaybe fallback (lookupConfig l.world key)

-- | The machine, in words.
-- |
-- | Two readings, and the honest thing about both is that neither is a second
-- | description kept in step by hand — the artifact is the decoded value
-- | re-encoded, and the text is derived from the same resolution of the same
-- | rules that produced the picture. Three renderings, one value.
textBlock :: forall m. State -> Loaded -> H.ComponentHTML Action () m
textBlock state l =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_
        [ HH.button
            [ HE.onClick \_ -> ToggleText, HP.class_ (H.ClassName "disclose") ]
            [ HH.text (if state.textOpen then "▾ In words" else "▸ In words") ]
        ]
    , if not state.textOpen then HH.text ""
      else HH.div_
        [ HH.div [ HP.class_ (H.ClassName "buttons") ]
            [ tab AsArtifact "the artifact"
            , tab AsDiagramText "as diagram text"
            ]
        , HH.pre_ [ HH.text body ]
        , HH.p [ HP.class_ (H.ClassName "quiet") ] [ HH.text gloss ]
        ]
    ]
  where
  tab view label =
    HH.button
      [ HE.onClick \_ -> SetTextView view
      , HP.class_ (H.ClassName (if state.textView == view then "on" else ""))
      ]
      [ HH.text label ]

  body = case state.textView of
    AsArtifact -> printSpecPretty l.spec
    AsDiagramText -> toStateDiagram (annotated state l)

  gloss = case state.textView of
    AsArtifact ->
      "The machine as the decoder understood it, re-encoded — not the bytes that arrived. \
      \If this differs from the file on disk, the codec lost something."
    AsDiagramText ->
      "The same machine as text — derived from the same rules as the picture, so \
      \it changes when the configuration does. Refusals are left out, since as \
      \self-loops they would treble the edge count to say what the styling \
      \already says. Mermaid stateDiagram-v2 syntax, so GitHub renders it if you \
      \paste it; nothing here depends on Mermaid to draw anything."

shapeBlock :: forall m. State -> H.ComponentHTML Action () m
shapeBlock state =
  HH.div [ HP.class_ (H.ClassName "block") ]
    [ HH.h2_ [ HH.text "Drawing" ]
    , HH.div [ HP.class_ (H.ClassName "buttons") ]
        [ HH.button
            [ HE.onClick \_ -> SetLayout AsTree
            , HP.class_ (H.ClassName (if state.layoutMode == AsTree then "on" else ""))
            ]
            [ HH.text "as the user meets it" ]
        , HH.button
            [ HE.onClick \_ -> SetLayout AsRing
            , HP.class_ (H.ClassName (if state.layoutMode == AsRing then "on" else ""))
            ]
            [ HH.text "as a graph" ]
        ]
    , HH.label [ HP.class_ (H.ClassName "toggle") ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.showRefusals
            , HE.onChecked \_ -> ToggleRefusals
            ]
        , HH.text "show refusals"
        ]
    ]
