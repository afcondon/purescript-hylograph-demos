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
  , globalDagTree
  , skylineTree
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
import Data.Number (sqrt) as Number
import Data.Set (Set)
import Data.Set as Set
import Hylograph.HATS (Tree, elem, onClick, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))
import Data.Tuple.Nested (type (/\), (/\))
import Baskerville.Explain (depthOf, explainFact)
import Baskerville.Kernel (Fact, FactId(..), Why(..), facts)
import Sudoku.Board (Cell(..), Digit(..), allCells, colOf, rowOf, units)
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

-- | Every premise edge of the whole solution — the one global DAG all
-- | per-cell proofs are projections of.
allEdges :: Array { from :: FactId, to :: FactId }
allEdges = facts reginKB # Array.concatMap \f -> case f.why of
  Axiom _ -> []
  ByRule d -> d.premises <#> \p -> { from: p, to: f.id }

-- | The ancestor cone of a fact: which of the solution's facts its proof
-- | rests on.
coneOf :: SudokuFact -> Set FactId
coneOf f = Set.fromFoldable (map _.id (explainFact reginKB f).nodes)

-- | Sizes for the caption: this cone, and the whole solution.
proofSummary :: SudokuFact -> { cone :: Int, total :: Int }
proofSummary f =
  { cone: Set.size (coneOf f)
  , total: Array.length (facts reginKB)
  }

-- | A laid-out proof scene: semantic groups, positions from Sugiyama,
-- | group-level edges, and bounds. Built once per node/edge set — the
-- | global scene is a top-level value, computed a single time.
type Scene =
  { groups :: Array (GKey /\ Array SudokuFact)
  , pos :: Map GKey { x :: Number, y :: Number }
  , gEdges :: Array (GKey /\ GKey)
  , minX :: Number
  , minY :: Number
  , spanW :: Number
  , spanH :: Number
  }

buildScene :: Maybe FactId -> Array SudokuFact -> Array { from :: FactId, to :: FactId } -> Scene
buildScene rootId sceneFacts sceneEdges =
  let
    depths = depthOf reginKB
    depthFor f = fromMaybe 0 (Map.lookup f.id depths)

    byId :: Map FactId SudokuFact
    byId = Map.fromFoldable (sceneFacts <#> \f -> f.id /\ f)

    localOut :: Map FactId (Array FactId)
    localOut = foldl (\m e -> Map.insertWith (<>) e.from [ e.to ] m) Map.empty sceneEdges

    -- semantic pass: a hidden single's premises, when they are all
    -- eliminations of one digit inside one house with no path between
    -- them, become one "no other d in <house>" node
    semantic :: Map FactId GKey
    semantic = foldl claimFor Map.empty sceneFacts
      where
      claimFor acc f = case f.why of
        ByRule d | d.rule == HiddenSingle ->
          let
            ps = d.premises # Array.mapMaybe (\id -> Map.lookup id byId)
            digits = ps # Array.mapMaybe \pf -> case pf.claim of
              Not _ dd -> Just dd
              Is _ _ -> Nothing
            cells = ps <#> \pf -> cellOfClaim pf.claim
            dep = fromMaybe 0 (maximum (ps <#> depthFor))
          in
            case Array.head digits of
              Just dd
                | Array.length digits == Array.length ps
                    && Array.all (_ == dd) digits
                    && Array.length ps >= 3
                    && not (interPath ps) ->
                    case unitIndexContaining (Array.cons (cellOfClaim f.claim) cells) of
                      Just h -> foldl
                        ( \a pf -> Map.alter
                            ( case _ of
                                Just k -> Just k
                                Nothing -> Just (GHouse h dd dep)
                            )
                            pf.id
                            a
                        )
                        acc
                        ps
                      Nothing -> acc
              _ -> acc
        _ -> acc

      cellOfClaim = case _ of
        Is c _ -> c
        Not c _ -> c

      -- contraction is safe iff no member reaches another through the
      -- scene's edges (direct member-member edges reduce to dropped
      -- self-loops and are harmless)
      interPath ps =
        let
          memberIds = Set.fromFoldable (ps <#> _.id)
          step frontierIds visited =
            let
              nextIds = frontierIds
                # Array.concatMap (\id -> fromMaybe [] (Map.lookup id localOut))
                # Array.filter (\id -> not (Set.member id visited))
                # Array.nub
            in
              if Array.null nextIds then false
              else if Array.any (\id -> Set.member id memberIds) nextIds then true
              else step nextIds (foldl (flip Set.insert) visited nextIds)
        in
          ps # Array.any \pf ->
            step (fromMaybe [] (Map.lookup pf.id localOut)) (Set.singleton pf.id)

    key :: SudokuFact -> GKey
    key f = case Map.lookup f.id semantic of
      Just k | Just f.id /= rootId -> k
      _ -> case f.claim of
        Not c _ | Just f.id /= rootId -> GGroup c (depthFor f)
        _ -> GSingle f.id

    groups :: Map GKey (Array SudokuFact)
    groups = foldl (\m f -> Map.insertWith (flip (<>)) (key f) [ f ] m) Map.empty sceneFacts

    keyOfId id = fromMaybe (GSingle id) (key <$> Map.lookup id byId)

    gEdgeSet :: Set (GKey /\ GKey)
    gEdgeSet = Set.fromFoldable
      ( sceneEdges
          # map (\e -> keyOfId e.from /\ keyOfId e.to)
          # Array.filter (\(a /\ b) -> a /= b)
      )

    gKeys :: Array GKey
    gKeys = Set.toUnfoldable (Map.keys groups)

    adjacency :: Map GKey (Set GKey)
    adjacency = foldl
      (\m (a /\ b) -> Map.insertWith Set.union a (Set.singleton b) m)
      (Map.fromFoldable (gKeys <#> \k -> k /\ Set.empty))
      (Set.toUnfoldable gEdgeSet :: Array (GKey /\ GKey))

    layout = sugiyamaLayout
      { nodeWidth: 40.0, nodeHeight: 200.0, orientation: Horizontal, reversed: false }
      gKeys
      adjacency

    pos :: Map GKey { x :: Number, y :: Number }
    pos = Map.fromFoldable (layout <#> \n -> n.id /\ { x: n.x, y: n.y })

    maxX = fromMaybe 0.0 (maximum (map _.x layout))
    maxY = fromMaybe 0.0 (maximum (map _.y layout))
    minX = fromMaybe 0.0 (minimum (map _.x layout))
    minY = fromMaybe 0.0 (minimum (map _.y layout))
  in
    { groups: Map.toUnfoldable groups
    , pos
    , gEdges: Set.toUnfoldable gEdgeSet
    , minX
    , minY
    , spanW: (maxX - minX) + nodeW + 80.0
    , spanH: (maxY - minY) + 100.0
    }

nodeW :: Number
nodeW = 150.0

-- | Draw a scene: everything lit, or a cone lit and the rest ghosted.
renderScene :: { emphasis :: Maybe (Set FactId), rootId :: Maybe FactId } -> Scene -> Tree
renderScene opts scene =
  let
    litGroup members = case opts.emphasis of
      Nothing -> true
      Just cone -> members # Array.any \f -> Set.member f.id cone

    litByKey :: Map GKey Boolean
    litByKey = Map.fromFoldable (scene.groups <#> \(k /\ members) -> k /\ litGroup members)

    isLit k = fromMaybe false (Map.lookup k litByKey)

    posFor k = fromMaybe { x: 0.0, y: 0.0 } (Map.lookup k scene.pos)

    edges = scene.gEdges <#> \(a /\ b) ->
      let
        from = posFor a
        to = posFor b
        bothLit = isLit a && isLit b
      in
        elem Line
          [ F.x1 (from.x + nodeW), F.y1 (from.y + 11.0)
          , F.x2 to.x, F.y2 (to.y + 11.0)
          , F.stroke "#c9c9c9", F.strokeWidth 1.0
          , staticStr "opacity" (if bothLit then "1" else "0.22")
          ]
          []

    nodes = scene.groups # Array.concatMap \(k /\ members) ->
      let
        p = posFor k
        isRoot = case k of
          GSingle id -> Just id == opts.rootId
          GGroup _ _ -> false
          GHouse _ _ _ -> false
        style = groupStyle members
      in
        [ elem Group
            [ staticStr "opacity" (if isLit k then "1" else "0.45") ]
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
        ]
  in
    elem SVG
      [ F.viewBox (scene.minX - 40.0) (scene.minY - 50.0) scene.spanW scene.spanH
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (edges <> nodes)

-- | The one global scene: the entire solution, laid out once.
globalScene :: Scene
globalScene = buildScene Nothing (facts reginKB) allEdges

-- | The whole solution with one cell's cone lit in place.
globalDagTree :: Maybe SudokuFact -> Tree
globalDagTree sel = renderScene
  { emphasis: coneOf <$> sel, rootId: _.id <$> sel }
  globalScene

-- | Pruned to one proof: the cone re-laid-out on its own.
dagTreeFor :: SudokuFact -> Tree
dagTreeFor root =
  let
    dag = explainFact reginKB root
  in
    renderScene { emphasis: Nothing, rootId: Just root.id }
      (buildScene (Just root.id) dag.nodes dag.edges)

-- | A rendered node: one fact, one cell's eliminations at one depth, or
-- | a semantic unit — a whole house's same-digit eliminations serving one
-- | hidden single, read as "no other 5 in box 5".
data GKey = GSingle FactId | GGroup Cell Int | GHouse Int Digit Int

derive instance Eq GKey
derive instance Ord GKey

-- | The first house (row, column, box — in that order) containing every
-- | listed cell, if any.
unitIndexContaining :: Array Cell -> Maybe Int
unitIndexContaining cells =
  units # Array.findIndex \house -> cells # Array.all \c -> Array.elem c house

houseName :: Int -> String
houseName i
  | i < 9 = "row " <> show (i + 1)
  | i < 18 = "column " <> show (i - 8)
  | otherwise = "box " <> show (i - 17)

groupLabel :: GKey -> Array SudokuFact -> String
groupLabel k members = case k, members of
  GHouse h d _, _ -> "no other " <> digitLabelOf d <> " in " <> houseName h
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

digitLabelOf :: Digit -> String
digitLabelOf (Digit d) = show d

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

-- | Proof size per cell: how many facts the full derivation of that
-- | placement rests on. The demo's other landscape.
proofSizes :: Map Cell Int
proofSizes = Map.fromFoldable
  ( allCells # Array.mapMaybe \c ->
      placementFor c <#> \f -> c /\ Array.length (explainFact reginKB f).nodes
  )

-- | The derivation city: an isometric transform of the board, each cell
-- | a prism whose height is log-scaled proof size. Gratuitous, and
-- | wonderful. Painter's order: back-to-front along the anti-diagonals.
skylineTree :: (Cell -> Effect Unit) -> Maybe Cell -> Tree
skylineTree notify selected =
  let
    u = 30.0
    ux = u * 0.866
    uy = u * 0.5
    maxN = fromMaybe 1 (maximum (Array.fromFoldable (Map.values proofSizes)))
    heightFor n =
      4.0 + 62.0 * Number.sqrt (Int.toNumber n / Int.toNumber maxN)

    projX r c = (Int.toNumber c - Int.toNumber r) * ux
    projY r c = (Int.toNumber c + Int.toNumber r) * uy

    pt x y = show x <> "," <> show y

    prism c =
      let
        r = rowOf c
        k = colOf c
        n = fromMaybe 1 (Map.lookup c proofSizes)
        h = heightFor n
        isSelected = selected == Just c
        shades = tierShades (tierOf c)
        -- projected corners of the cell footprint
        x00 = projX r k
        y00 = projY r k
        x01 = projX r (k + 1)
        y01 = projY r (k + 1)
        x11 = projX (r + 1) (k + 1)
        y11 = projY (r + 1) (k + 1)
        x10 = projX (r + 1) k
        y10 = projY (r + 1) k
        face points fill =
          elem Polygon
            [ staticStr "points" points
            , F.fill fill
            , F.stroke (if isSelected then "#111111" else shades.line)
            , F.strokeWidth (if isSelected then 1.5 else 0.4)
            ]
            []
        tooltip = "r" <> show (rowOf c + 1) <> "c" <> show (colOf c + 1)
          <> " \x2014 proof of " <> show n <> " facts"
      in
        withBehaviors [ onClick (notify c) ] $ elem Group
          [ staticStr "cursor" "pointer" ]
          [ elem Title [ staticStr "textContent" tooltip ] []
          -- front face (row r+1 edge)
          , face
              ( pt x10 (y10 - h) <> " " <> pt x11 (y11 - h) <> " "
                  <> pt x11 y11 <> " " <> pt x10 y10
              )
              shades.front
          -- right face (column k+1 edge)
          , face
              ( pt x01 (y01 - h) <> " " <> pt x11 (y11 - h) <> " "
                  <> pt x11 y11 <> " " <> pt x01 y01
              )
              shades.side
          -- top face
          , face
              ( pt x00 (y00 - h) <> " " <> pt x01 (y01 - h) <> " "
                  <> pt x11 (y11 - h) <> " " <> pt x10 (y10 - h)
              )
              shades.top
          ]

    -- back-to-front: anti-diagonal order, so nearer prisms overpaint
    ordered = allCells # Array.sortBy (comparing \c -> rowOf c + colOf c)

    minX = -8.0 * ux - 20.0
    width = 17.0 * ux + 40.0
    maxH = 4.0 + 44.0
    height = 16.0 * uy + uy * 2.0 + maxH + 40.0
  in
    elem SVG
      [ F.viewBox minX (-maxH - 20.0) width height
      , F.width width
      , F.height height
      ]
      (map prism ordered)

tierShades :: Tier -> { top :: String, front :: String, side :: String, line :: String }
tierShades = case _ of
  TGiven -> { top: "#e0e0e0", front: "#c4c4c4", side: "#adadad", line: "#909090" }
  TSingles -> { top: "#7aa6c2", front: "#6590ac", side: "#527d99", line: "#3f637c" }
  TRegin -> { top: "#e89a44", front: "#cd8232", side: "#b26e26", line: "#8f5717" }
