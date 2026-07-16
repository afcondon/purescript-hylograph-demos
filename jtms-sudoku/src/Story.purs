-- | The pure computation behind the demo: solve the oracle-discovered gap
-- | puzzle with both engines, colour the grid by which tier earned each
-- | cell, and lay out one Régin-bearing proof DAG with hylograph-graph's
-- | Sugiyama — jtms facts in, hylograph geometry out.
module Story
  ( SudokuFact
  , gridTree
  , dagTreeFor
  , defaultDagFact
  , placementFor
  , factTitle
  , proofSummary
  , stats
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximum, minimum, minimumBy)
import Data.Graph.Layout (TreeLayout(..), sugiyamaLayout)
import Data.Int as Int
import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Hylograph.HATS (Tree, elem, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))
import Data.Tuple.Nested (type (/\), (/\))
import Jtms.Explain (DerivationDag, depthOf, explainFact)
import Jtms.Kernel (Fact, FactId(..), Why(..), facts)
import Sudoku.Board (Cell(..), Digit(..), allCells, colOf, rowOf)
import Sudoku.Fixtures (gapPuzzle)
import Sudoku.Rules (Given(..), SudokuClaim(..), SudokuKB, SudokuRule(..), alldifferentEngine, engine)
import Sudoku.Solve (solveWith, solvedCount, valueAt)

type SudokuFact = Fact SudokuClaim SudokuRule Given

singlesKB :: SudokuKB
singlesKB = solveWith engine gapPuzzle

reginKB :: SudokuKB
reginKB = solveWith alldifferentEngine gapPuzzle

givenCells :: Set Cell
givenCells = Set.fromFoldable (map _.cell gapPuzzle)

stats :: { givens :: Int, singles :: Int, regin :: Int }
stats =
  { givens: Array.length gapPuzzle
  , singles: solvedCount singlesKB - Array.length gapPuzzle
  , regin: solvedCount reginKB - solvedCount singlesKB
  }

-- | Which tier earned each cell of the (fully solved) grid.
data Tier = TGiven | TSingles | TRegin

tierOf :: Cell -> Tier
tierOf c
  | Set.member c givenCells = TGiven
  | isJust (valueAt singlesKB c) = TSingles
  | otherwise = TRegin

tierFill :: Tier -> String
tierFill = case _ of
  TGiven -> "#f2f2f2"
  TSingles -> "#e8f0f6"
  TRegin -> "#fff4e5"

-- | The 9×9 grid, cells coloured by tier — the completeness gap, visible.
-- | Every cell is clickable: the handler receives the cell, and the
-- | selected one wears a strong outline.
gridTree :: (Cell -> Effect Unit) -> Maybe Cell -> Tree
gridTree notify selected =
  let
    px = 40.0
    size = 9.0 * px + 2.0
    squares = allCells # map \c ->
      let
        x = Int.toNumber (colOf c) * px + 1.0
        y = Int.toNumber (rowOf c) * px + 1.0
        isSelected = selected == Just c
      in
        withBehaviors [ onClick (notify c) ] $ elem Group
          [ staticStr "cursor" "pointer" ]
          [ elem Rect
              [ F.x x, F.y y, F.width px, F.height px
              , F.fill (tierFill (tierOf c))
              , F.stroke (if isSelected then "#111111" else "#dddddd")
              , F.strokeWidth (if isSelected then 2.5 else 0.5)
              ]
              []
          , elem Text
              [ F.x (x + px / 2.0), F.y (y + px / 2.0 + 5.0)
              , F.textAnchor "middle"
              , F.fontSize "16"
              , F.fontFamily "'Helvetica Neue', Helvetica, sans-serif"
              , F.fill "#111111"
              , staticStr "textContent" (digitAt c)
              ]
              []
          ]
    boxLines = [ 0, 3, 6, 9 ] # Array.concatMap \i ->
      let
        p = Int.toNumber i * px + 1.0
      in
        [ elem Line
            [ F.x1 p, F.y1 1.0, F.x2 p, F.y2 (size - 1.0)
            , F.stroke "#999999", F.strokeWidth 1.5
            ]
            []
        , elem Line
            [ F.x1 1.0, F.y1 p, F.x2 (size - 1.0), F.y2 p
            , F.stroke "#999999", F.strokeWidth 1.5
            ]
            []
        ]
  in
    elem SVG
      [ F.viewBox 0.0 0.0 size size, F.width size, F.height size ]
      (squares <> boxLines)
  where
  digitAt c = case valueAt reginKB c of
    Just (Digit d) -> show d
    Nothing -> ""

-- | The smallest proof that needed the algorithmic tier, laid out by
-- | hylograph-graph's Sugiyama.
-- |
-- | View-level compaction: elimination facts (`Not`) sharing a cell AND a
-- | proof depth collapse into one node — "r2c5 \x2260 {1,2,3,4,6,7,8}" — the
-- | proof keeps its individual facts, only the picture groups them. The
-- | (cell, depth) key is what makes contraction safe: same-depth nodes
-- | can have no path between them, so no cycle can be created.
-- | Premises of a fact (empty for axioms).
premisesOf :: SudokuFact -> Array FactId
premisesOf f = case f.why of
  Axiom _ -> []
  ByRule d -> d.premises

-- | The proximate proof: the conclusion and its support out to a hop
-- | budget, chosen adaptively so the picture stays legible. Facts at the
-- | cut whose support continues beyond it are marked (dashed, with an
-- | ellipsis) rather than silently pretending to be leaves.
localProof
  :: SudokuFact
  -> { nodes :: Array SudokuFact
     , edges :: Array { from :: FactId, to :: FactId }
     , frontier :: Set FactId
     , total :: Int
     }
localProof root =
  let
    dag = explainFact reginKB root
    byId = Map.fromFoldable (dag.nodes <#> \f -> f.id /\ f)
    lookupF id = Map.lookup id byId

    grow kept current 0 = { kept, next: current }
    grow kept current n =
      let
        next = current
          # Array.concatMap premisesOf
          # Array.filter (\id -> not (Set.member id (Set.fromFoldable (map _.id kept))))
          # Array.nub
          # Array.mapMaybe lookupF
      in
        if Array.null next then { kept, next: [] }
        else grow (kept <> next) next (n - 1)

    keptFor hops = (grow [ root ] [ root ] hops).kept

    pick hops
      | hops <= 1 = keptFor 1
      | otherwise =
          let attempt = keptFor hops
          in if Array.length attempt <= 60 then attempt else pick (hops - 1)

    kept = pick 4
    keptIds = Set.fromFoldable (map _.id kept)
    edges = dag.edges # Array.filter \e ->
      Set.member e.from keptIds && Set.member e.to keptIds
    frontier = kept
      # Array.filter (\f -> premisesOf f # Array.any (\p -> not (Set.member p keptIds)))
      # map _.id
      # Set.fromFoldable
  in
    { nodes: kept, edges, frontier, total: Array.length dag.nodes }

-- | "34 of 214 facts" — the App's honesty line under the proof heading.
proofSummary :: SudokuFact -> { shown :: Int, total :: Int }
proofSummary f =
  let local = localProof f
  in { shown: Array.length local.nodes, total: local.total }

dagTreeFor :: SudokuFact -> Tree
dagTreeFor root =
  let
    dag = localProof root
    depths = depthOf reginKB
    depthFor f = fromMaybe 0 (Map.lookup f.id depths)

    key :: SudokuFact -> GKey
    key f = case f.claim of
      Not c _ | f.id /= root.id -> GGroup c (depthFor f)
      _ -> GSingle f.id

    byId :: Map FactId SudokuFact
    byId = Map.fromFoldable (dag.nodes <#> \f -> f.id /\ f)

    groups :: Map GKey (Array SudokuFact)
    groups = foldl (\m f -> Map.insertWith (flip (<>)) (key f) [ f ] m) Map.empty dag.nodes

    keyOfId id = fromMaybe (GSingle id) (key <$> Map.lookup id byId)

    gEdges :: Set (GKey /\ GKey)
    gEdges = Set.fromFoldable
      ( dag.edges
          # map (\e -> keyOfId e.from /\ keyOfId e.to)
          # Array.filter (\(a /\ b) -> a /= b)
      )

    adjacency :: Map GKey (Set GKey)
    adjacency = foldl
      (\m (a /\ b) -> Map.insertWith Set.union a (Set.singleton b) m)
      (Map.fromFoldable (gKeys <#> \k -> k /\ Set.empty))
      (Set.toUnfoldable gEdges :: Array (GKey /\ GKey))

    gKeys :: Array GKey
    gKeys = Set.toUnfoldable (Map.keys groups)

    layout = sugiyamaLayout
      { nodeWidth: 40.0, nodeHeight: 200.0, orientation: Horizontal, reversed: false }
      gKeys
      adjacency

    pos :: Map GKey { x :: Number, y :: Number }
    pos = Map.fromFoldable (layout <#> \n -> n.id /\ { x: n.x, y: n.y })

    posFor k = fromMaybe { x: 0.0, y: 0.0 } (Map.lookup k pos)

    maxX = fromMaybe 0.0 (maximum (map _.x layout))
    maxY = fromMaybe 0.0 (maximum (map _.y layout))
    minX = fromMaybe 0.0 (minimum (map _.x layout))
    minY = fromMaybe 0.0 (minimum (map _.y layout))
    nodeW = 150.0
    spanW = (maxX - minX) + nodeW + 80.0
    spanH = (maxY - minY) + 100.0

    edges = (Set.toUnfoldable gEdges :: Array (GKey /\ GKey)) # map \(a /\ b) ->
      let
        from = posFor a
        to = posFor b
      in
        elem Line
          [ F.x1 (from.x + nodeW), F.y1 (from.y + 11.0)
          , F.x2 to.x, F.y2 (to.y + 11.0)
          , F.stroke "#c9c9c9", F.strokeWidth 1.0
          ]
          []

    nodes = (Map.toUnfoldable groups :: Array (GKey /\ Array SudokuFact))
      # Array.concatMap \(k /\ members) ->
          let
            p = posFor k
            isRoot = case k of
              GSingle id -> id == root.id
              GGroup _ _ -> false
            onFrontier = members # Array.any (\f -> Set.member f.id dag.frontier)
            style = groupStyle members
          in
            [ elem Rect
                ( [ F.x p.x, F.y p.y, F.width nodeW, F.height 22.0
                  , staticStr "rx" "3"
                  , F.fill style.fill, F.stroke style.stroke
                  , F.strokeWidth (if isRoot then 2.5 else 1.0)
                  ] <> (if onFrontier then [ staticStr "stroke-dasharray" "5,3" ] else [])
                )
                []
            , elem Text
                [ F.x (p.x + 8.0), F.y (p.y + 15.0)
                , F.fontSize "11"
                , F.fontFamily "'Helvetica Neue', Helvetica, sans-serif"
                , F.fill "#111111"
                , staticStr "textContent"
                    (groupLabel k members <> (if onFrontier then " \x22ef" else ""))
                ]
                []
            ]
  in
    elem SVG
      [ F.viewBox (minX - 40.0) (minY - 50.0) spanW spanH
      , F.width spanW
      , F.height spanH
      ]
      (edges <> nodes)

-- | A rendered node: one fact, or one cell's eliminations at one depth.
data GKey = GSingle FactId | GGroup Cell Int

derive instance Eq GKey
derive instance Ord GKey

groupLabel :: GKey -> Array SudokuFact -> String
groupLabel k members = case k, members of
  GSingle _, [ f ] -> claimLabel f.claim
  GGroup c _, _ -> case members # Array.mapMaybe digitOf of
    [ d ] -> cellNameOf c <> " \x2260 " <> show d
    ds -> cellNameOf c <> " \x2260 {" <> joinDigits ds <> "}"
  GSingle _, _ -> "?"
  where
  digitOf f = case f.claim of
    Not _ (Digit d) -> Just d
    Is _ _ -> Nothing
  joinDigits ds = Array.intercalate "," (map show (Array.sort ds))
  cellNameOf c = "r" <> show (rowOf c + 1) <> "c" <> show (colOf c + 1)

-- | Uniform-rule groups keep their rule's colours; mixed provenance reads
-- | as neutral.
groupStyle :: Array SudokuFact -> { fill :: String, stroke :: String }
groupStyle members = case Array.head members of
  Nothing -> { fill: "#ffffff", stroke: "#999999" }
  Just first ->
    if Array.all (\f -> ruleTag f == ruleTag first) members then
      { fill: fillFor first, stroke: strokeFor first }
    else
      { fill: "#f7f7f7", stroke: "#666666" }
  where
  ruleTag f = case f.why of
    Axiom _ -> Nothing
    ByRule d -> Just d.rule

-- | The default selection: the deduced placement with the smallest proof
-- | that contains an Alldifferent step.
defaultDagFact :: SudokuFact
defaultDagFact =
  let
    deduced = facts reginKB # Array.filter \f -> case f.claim, f.why of
      Is _ _, ByRule _ -> true
      _, _ -> false
    withAmber = deduced # Array.filter \f ->
      Array.any isAmber (explainFact reginKB f).nodes
  in
    case minimumBy (comparing (\f -> Array.length (explainFact reginKB f).nodes)) withAmber of
      Just f -> f
      -- unreachable on this fixture (Régin demonstrably participates)
      Nothing -> case Array.head (facts reginKB) of
        Just f -> f
        Nothing -> { id: FactId 0, claim: Is (Cell 0) (Digit 1), why: Axiom Given, alsoWhy: [] }
  where
  isAmber f = case f.why of
    ByRule d -> d.rule == Alldifferent
    Axiom _ -> false

-- | The placement fact recorded for a cell (every cell has one — the
-- | alldifferent engine solves this puzzle completely).
placementFor :: Cell -> Maybe SudokuFact
placementFor c = facts reginKB # Array.find \f -> case f.claim of
  Is c' _ -> c' == c
  Not _ _ -> false

-- | "r7c4 = 3, by hidden single" — the DAG panel's live heading.
factTitle :: SudokuFact -> String
factTitle f = claimLabel f.claim <> byline
  where
  byline = case f.why of
    Axiom _ -> ", a given"
    ByRule d -> ", by " <> ruleNameOf d.rule
  ruleNameOf = case _ of
    SoleDigit -> "sole digit"
    PeerElimination -> "peer elimination"
    NakedSingle -> "naked single"
    HiddenSingle -> "hidden single"
    Alldifferent -> "alldifferent (R\xe9gin)"

claimLabel :: SudokuClaim -> String
claimLabel = case _ of
  Is c d -> cellName c <> " = " <> digitLabel d
  Not c d -> cellName c <> " \x2260 " <> digitLabel d
  where
  digitLabel (Digit d) = show d
  cellName c = "r" <> show (rowOf c + 1) <> "c" <> show (colOf c + 1)

fillFor :: SudokuFact -> String
fillFor f = case f.why of
  Axiom _ -> "#f2f2f2"
  ByRule d -> case d.rule of
    SoleDigit -> "#e8f0f6"
    PeerElimination -> "#eef3ee"
    NakedSingle -> "#fdeeed"
    HiddenSingle -> "#ececf4"
    Alldifferent -> "#fff4e5"

strokeFor :: SudokuFact -> String
strokeFor f = case f.why of
  Axiom _ -> "#999999"
  ByRule d -> case d.rule of
    SoleDigit -> "#7aa6c2"
    PeerElimination -> "#7fa87f"
    NakedSingle -> "#d64541"
    HiddenSingle -> "#5b5ba6"
    Alldifferent -> "#e08b2d"
