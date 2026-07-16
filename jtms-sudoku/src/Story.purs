-- | The pure computation behind the demo: solve the oracle-discovered gap
-- | puzzle with both engines, colour the grid by which tier earned each
-- | cell, and lay out one Régin-bearing proof DAG with hylograph-graph's
-- | Sugiyama — jtms facts in, hylograph geometry out.
module Story
  ( gridTree
  , dagTree
  , stats
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, maximum, minimumBy)
import Data.Graph.Layout (TreeLayout(..), sugiyamaLayout)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Hylograph.HATS (Tree, elem, staticStr)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))
import Data.Tuple.Nested (type (/\), (/\))
import Jtms.Explain (DerivationDag, depthOf, explainFact)
import Jtms.Kernel (Fact, FactId(..), Why(..), facts)
import Sudoku.Board (Cell(..), Digit(..), allCells, colOf, rowOf)
import Sudoku.Fixtures (gapPuzzle)
import Sudoku.Rules (Given, SudokuClaim(..), SudokuKB, SudokuRule(..), alldifferentEngine, engine)
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
gridTree :: Tree
gridTree =
  let
    px = 40.0
    size = 9.0 * px + 2.0
    squares = allCells # Array.concatMap \c ->
      let
        x = Int.toNumber (colOf c) * px + 1.0
        y = Int.toNumber (rowOf c) * px + 1.0
      in
        [ elem Rect
            [ F.x x, F.y y, F.width px, F.height px
            , F.fill (tierFill (tierOf c))
            , F.stroke "#dddddd", F.strokeWidth 0.5
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
dagTree :: Tree
dagTree =
  let
    dag = chosenDag
    depths = depthOf reginKB
    depthFor f = fromMaybe 0 (Map.lookup f.id depths)

    key :: SudokuFact -> GKey
    key f = case f.claim of
      Not c _ | f.id /= dag.root -> GGroup c (depthFor f)
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
      { nodeWidth: 34.0, nodeHeight: 200.0, orientation: Horizontal, reversed: false }
      gKeys
      adjacency

    pos :: Map GKey { x :: Number, y :: Number }
    pos = Map.fromFoldable (layout <#> \n -> n.id /\ { x: n.x, y: n.y })

    posFor k = fromMaybe { x: 0.0, y: 0.0 } (Map.lookup k pos)

    maxX = fromMaybe 0.0 (maximum (map _.x layout))
    maxY = fromMaybe 0.0 (maximum (map _.y layout))
    nodeW = 150.0
    width = maxX + nodeW + 40.0
    height = maxY + 60.0

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
              GSingle id -> id == dag.root
              GGroup _ _ -> false
            style = groupStyle members
          in
            [ elem Rect
                [ F.x p.x, F.y p.y, F.width nodeW, F.height 22.0
                , staticStr "rx" "3"
                , F.fill style.fill, F.stroke style.stroke
                , F.strokeWidth (if isRoot then 2.5 else 1.0)
                ]
                []
            , elem Text
                [ F.x (p.x + 8.0), F.y (p.y + 15.0)
                , F.fontSize "11"
                , F.fontFamily "'Helvetica Neue', Helvetica, sans-serif"
                , F.fill "#111111"
                , staticStr "textContent" (groupLabel k members)
                ]
                []
            ]
  in
    elem SVG
      [ F.viewBox (-20.0) (-20.0) (width + 40.0) (height + 40.0)
      , F.width (width + 40.0)
      , F.height (height + 40.0)
      ]
      (edges <> nodes)

-- | A rendered node: one fact, or one cell's eliminations at one depth.
data GKey = GSingle FactId | GGroup Cell Int

derive instance Eq GKey
derive instance Ord GKey

groupLabel :: GKey -> Array SudokuFact -> String
groupLabel k members = case k, members of
  GSingle _, [ f ] -> claimLabel f.claim
  GGroup c _, _ ->
    cellNameOf c <> " \x2260 {"
      <> joinDigits (members # Array.mapMaybe digitOf)
      <> "}"
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

-- | Deduced placements whose proof contains an Alldifferent step; the
-- | smallest such proof is the one we can label.
chosenDag :: DerivationDag SudokuClaim SudokuRule Given
chosenDag =
  let
    deduced = facts reginKB # Array.filter \f -> case f.claim, f.why of
      Is _ _, ByRule _ -> true
      _, _ -> false
    withAmber = deduced
      # map (explainFact reginKB)
      # Array.filter (\dag -> Array.any isAmber dag.nodes)
  in
    case minimumBy (comparing (Array.length <<< _.nodes)) withAmber of
      Just dag -> dag
      -- unreachable on this fixture (Régin demonstrably participates);
      -- an empty dag renders as an empty svg rather than crashing
      Nothing -> { root: FactId 0, nodes: [], edges: [] }
  where
  isAmber f = case f.why of
    ByRule d -> d.rule == Alldifferent
    Axiom _ -> false

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
