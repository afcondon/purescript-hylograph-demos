-- | Honeycomb Puzzle - Halogen Component
-- |
-- | 6 hexagonal graphs with shared edge toggle mechanic.
-- | Each graph runs in its own force simulation (a stepped handle), and one
-- | animation loop for the page steps every graph that is still moving.
-- | Circle of Fifths node naming, paths chain through graphs.
module Honeycomb.Component where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Honeycomb.AnimationFrames (animationFrames)
import Honeycomb.Types (NodeId, EdgeKey, graphEndpoints)
import Honeycomb.Render as Render
import Honeycomb.Pathfinding as Path
import Honeycomb.Generator as Gen
import Honeycomb.Simulation as Sim

-- =============================================================================
-- Types
-- =============================================================================

-- | Per-graph puzzle state (not simulation state)
type GraphPuzzle =
  { index :: Int
  , baseEdges :: Set EdgeKey
  }

-- | Component state - ALL STATE LIVES IN HALOGEN
type State =
  { graphs :: Array GraphPuzzle
  , globalToggles :: Set EdgeKey
  , moveCount :: Int
  , scrambleHistory :: Array EdgeKey
  , paths :: Array (Maybe (Array NodeId))
  -- One layout per graph, in graph order: its simulation and positions
  , layouts :: Array Sim.Layout
  -- Whether "all paths connected" has been said, and waved away
  , announcement :: Announcement
  -- Whether the Solve button gave the answer
  , solutionShown :: Boolean
  }

-- | Where the solved dialog stands. It is announced when the puzzle becomes
-- | solved, stays dismissed while it remains solved, and starts again once
-- | the puzzle is unsolved.
data Announcement
  = Unsolved
  | Announced
  | Dismissed

derive instance Eq Announcement

-- | Component actions
data Action
  = Initialize
  | ToggleEdge EdgeKey
  | NewPuzzle
  | ResetPuzzle
  | SolvePuzzle
  | AnimationFrame
  | CheckPaths
  | DismissDialog

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall input. input -> State
initialState _ =
  { graphs: []
  , globalToggles: Set.empty
  , moveCount: 0
  , scrambleHistory: []
  , paths: Array.replicate 6 Nothing
  , layouts: []
  , announcement: Unsolved
  , solutionShown: false
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "honeycomb-container") ]
    [ renderHeader state
    , renderHoneycomb state
    , renderControls
    , renderInstructions
    , renderTechDescription
    , renderSolvedDialog state
    ]

renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div
    [ HP.class_ (H.ClassName "honeycomb-header") ]
    [ HH.h1_ [ HH.text "The Honeycomb" ]
    , HH.p
        [ HP.class_ (H.ClassName "subtitle") ]
        [ HH.text "Toggle an edge in one graph, and it toggles in all six." ]
    , renderLegend
    , HH.div
        [ HP.class_ (H.ClassName "status-bar") ]
        [ HH.span_ [ HH.text "Paths connected: " ]
        , HH.span
            [ HP.class_ (H.ClassName $ if connectedCount == 6 then "count complete" else "count") ]
            [ HH.text $ show connectedCount <> " / 6" ]
        , HH.span_ [ HH.text $ " | Moves: " <> show state.moveCount ]
        ]
    ]
  where
  connectedCount = Array.length $ Array.filter isJust state.paths
  isJust (Just _) = true
  isJust Nothing = false

renderHoneycomb :: forall m. State -> H.ComponentHTML Action () m
renderHoneycomb state =
  HH.div
    [ HP.class_ (H.ClassName "honeycomb-grid") ]
    (Array.mapWithIndex (renderHexagon state) state.graphs)

renderHexagon :: forall m. State -> Int -> GraphPuzzle -> H.ComponentHTML Action () m
renderHexagon state idx graph =
  HH.div
    [ HP.class_ (H.ClassName $ "hex-container hex-" <> show idx) ]
    [ HH.div
        [ HP.class_ (H.ClassName $ "hexagon " <> statusClass) ]
        [ HH.span
            [ HP.class_ (H.ClassName "hex-label") ]
            [ HH.text $ "Graph " <> show (idx + 1) ]
        , HH.div
            [ HP.class_ (H.ClassName "graph-svg") ]
            [ Render.renderGraphSvg renderConfig ToggleEdge ]
        , HH.span
            [ HP.class_ (H.ClassName $ "hex-status " <> statusClass) ]
            [ HH.text statusText ]
        ]
    ]
  where
  mPath = Array.index state.paths idx # join
  hasPath = case mPath of
    Just _ -> true
    Nothing -> false
  statusClass = if hasPath then "connected" else "disconnected"
  statusText = if hasPath then "connected" else "no path"

  -- Get positions for this graph (from simulation or default)
  positions = fromMaybe Map.empty $ _.positions <$> Array.index state.layouts idx

  renderConfig = Render.mkRenderConfigWithPositions idx graph.baseEdges state.globalToggles mPath positions

  join :: forall a. Maybe (Maybe a) -> Maybe a
  join Nothing = Nothing
  join (Just x) = x

renderControls :: forall m. H.ComponentHTML Action () m
renderControls =
  HH.div
    [ HP.class_ (H.ClassName "controls") ]
    [ HH.button
        [ HE.onClick \_ -> NewPuzzle ]
        [ HH.text "New Puzzle" ]
    , HH.button
        [ HE.onClick \_ -> ResetPuzzle ]
        [ HH.text "Reset" ]
    , HH.button
        [ HP.class_ (H.ClassName "primary")
        , HE.onClick \_ -> SolvePuzzle
        ]
        [ HH.text "Solve" ]
    ]

renderInstructions :: forall m. H.ComponentHTML Action () m
renderInstructions =
  HH.p
    [ HP.class_ (H.ClassName "instructions") ]
    [ HH.text "Click edges to toggle. Find the configuration where all six paths connect." ]

renderTechDescription :: forall m. H.ComponentHTML Action () m
renderTechDescription =
  HH.div
    [ HP.class_ (H.ClassName "tech-description") ]
    [ HH.h3_ [ HH.text "How It Works" ]
    , HH.p_
        [ HH.text "Six graphs, each running its own "
        , HH.span [ HP.class_ (H.ClassName "highlight") ] [ HH.text "force-directed simulation" ]
        , HH.text ". We compute the shortest path from "
        , HH.span [ HP.class_ (H.ClassName "highlight") ] [ HH.text "source" ]
        , HH.text " (blue) to "
        , HH.span [ HP.class_ (H.ClassName "highlight") ] [ HH.text "target" ]
        , HH.text " (red) in each graph using BFS."
        ]
    , HH.p_
        [ HH.text "Toggle any edge and it toggles in "
        , HH.span [ HP.class_ (H.ClassName "highlight") ] [ HH.text "all six graphs" ]
        , HH.text " (XOR). The puzzle: find the configuration where every graph has a valid path."
        ]
    , HH.p_
        [ HH.text "Paths chain through the Circle of Fifths: C→G→D→A→E→B→F#"
        ]
    ]

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div
    [ HP.class_ (H.ClassName "legend") ]
    [ HH.div
        [ HP.class_ (H.ClassName "legend-item") ]
        [ HH.div [ HP.class_ (H.ClassName "legend-swatch start") ] []
        , HH.text "Source"
        ]
    , HH.div
        [ HP.class_ (H.ClassName "legend-item") ]
        [ HH.div [ HP.class_ (H.ClassName "legend-swatch end") ] []
        , HH.text "Target"
        ]
    , HH.div
        [ HP.class_ (H.ClassName "legend-item") ]
        [ HH.div [ HP.class_ (H.ClassName "legend-swatch path") ] []
        , HH.text "On path"
        ]
    , HH.div
        [ HP.class_ (H.ClassName "legend-item") ]
        [ HH.div [ HP.class_ (H.ClassName "legend-line active") ] []
        , HH.text "Edge"
        ]
    ]

-- | The dialog that says the puzzle is solved. It sits at the foot of the
-- | window and covers none of the graphs, so the solution stays in view and
-- | can still be played; it is drawn only while the puzzle is solved and
-- | has not been dismissed.
renderSolvedDialog :: forall m. State -> H.ComponentHTML Action () m
renderSolvedDialog state = case state.announcement of
  Unsolved -> HH.text ""
  Dismissed -> HH.text ""
  Announced ->
    HH.div
      [ HP.class_ (H.ClassName "solved-dialog")
      , HP.attr (HH.AttrName "role") "dialog"
      , HP.attr (HH.AttrName "aria-label") "All paths connected"
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "solved-words") ]
          [ HH.div [ HP.class_ (H.ClassName "win-text") ] [ HH.text "All Paths Connected" ]
          , HH.div [ HP.class_ (H.ClassName "win-moves") ] [ HH.text how ]
          ]
      , HH.button
          [ HP.class_ (H.ClassName "keep-looking")
          , HE.onClick \_ -> DismissDialog
          ]
          [ HH.text "Keep Looking" ]
      , HH.button
          [ HP.class_ (H.ClassName "play-again")
          , HE.onClick \_ -> NewPuzzle
          ]
          [ HH.text "Play Again" ]
      ]
  where
  how
    | state.solutionShown = "The solution, shown"
    | state.moveCount == 0 = "Connected from the start"
    | state.moveCount == 1 = "Solved in 1 move"
    | otherwise = "Solved in " <> show state.moveCount <> " moves"

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    puzzle <- liftEffect $ Gen.generatePuzzle 6
    H.modify_ _
      { graphs = puzzle.graphs <#> \g -> { index: g.index, baseEdges: g.baseEdges }
      , globalToggles = puzzle.initialToggles
      , scrambleHistory = puzzle.scrambleHistory
      }
    state <- H.get
    layouts <- liftEffect $ traverse Sim.createLayout (effectiveEdgesOf state)
    H.modify_ _ { layouts = layouts }
    void $ H.subscribe (animationFrames <#> const AnimationFrame)
    handleAction CheckPaths

  ToggleEdge edge -> do
    H.modify_ \s -> s
      { globalToggles = toggleInSet edge s.globalToggles
      , moveCount = s.moveCount + 1
      }
    relinkAll
    handleAction CheckPaths

  NewPuzzle -> do
    puzzle <- liftEffect $ Gen.generatePuzzle 6
    H.modify_ _
      { graphs = puzzle.graphs <#> \g -> { index: g.index, baseEdges: g.baseEdges }
      , globalToggles = puzzle.initialToggles
      , scrambleHistory = puzzle.scrambleHistory
      , moveCount = 0
      , solutionShown = false
      }
    relinkAll
    handleAction CheckPaths

  ResetPuzzle -> do
    state <- H.get
    let toggles = Array.foldl (flip toggleInSet) Set.empty state.scrambleHistory
    H.modify_ _ { globalToggles = toggles, moveCount = 0, solutionShown = false }
    relinkAll
    handleAction CheckPaths

  SolvePuzzle -> do
    H.modify_ _ { globalToggles = Set.empty, solutionShown = true }
    relinkAll
    handleAction CheckPaths

  AnimationFrame -> do
    layouts <- H.gets _.layouts
    when (Array.any _.moving layouts) do
      stepped <- liftEffect $ traverse Sim.stepLayout layouts
      H.modify_ _ { layouts = stepped }

  CheckPaths -> do
    state <- H.get
    let computedPaths = state.graphs <#> \g -> computePath g state.globalToggles
    let
      solved = not (Array.null computedPaths) && Array.all isJust computedPaths
      announcement = case state.announcement of
        _ | not solved -> Unsolved
        Unsolved -> Announced
        standing -> standing
    H.modify_ _ { paths = computedPaths, announcement = announcement }

  DismissDialog ->
    H.modify_ _ { announcement = Dismissed }

-- | Give every graph's simulation the edges the graph now has; those whose
-- | edges changed are reheated and start moving.
relinkAll :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
relinkAll = do
  state <- H.get
  layouts <- liftEffect $ relinkLayouts state
  H.modify_ _ { layouts = layouts }

relinkLayouts :: State -> Effect (Array Sim.Layout)
relinkLayouts state =
  traverse (\(Tuple edges layout) -> Sim.relinkLayout edges layout)
    (Array.zip (effectiveEdgesOf state) state.layouts)

-- =============================================================================
-- Helpers
-- =============================================================================

toggleInSet :: forall a. Ord a => a -> Set a -> Set a
toggleInSet x s = if Set.member x s then Set.delete x s else Set.insert x s

-- | Get effective edges (base XOR toggles)
getEffectiveEdges :: GraphPuzzle -> Set EdgeKey -> Set EdgeKey
getEffectiveEdges graph toggles =
  Set.union
    (Set.difference graph.baseEdges toggles)
    (Set.difference toggles graph.baseEdges)

-- | Compute path from start to end for a graph
computePath :: GraphPuzzle -> Set EdgeKey -> Maybe (Array NodeId)
computePath graph toggles =
  let
    effectiveEdges = getEffectiveEdges graph toggles
    endpoints = graphEndpoints graph.index
  in
    Path.findPath endpoints.start endpoints.end effectiveEdges

-- | Each graph's effective edges, in graph order.
effectiveEdgesOf :: forall r. { graphs :: Array GraphPuzzle, globalToggles :: Set EdgeKey | r } -> Array (Set EdgeKey)
effectiveEdgesOf state = state.graphs <#> \graph -> getEffectiveEdges graph state.globalToggles
