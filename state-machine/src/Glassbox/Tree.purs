-- | Glassbox.Tree
-- |
-- | The tree the user is forced to build, and everything that is not in it.
-- |
-- | A designer sees a graph, in which every transition is equally near to hand
-- | and hierarchy is a matter of taste. A user cannot see a graph: navigation is
-- | a walk from home and memory is hierarchical, so what a user ends up holding
-- | is the shortest-path tree rooted at the initial state, plus a pile of edges
-- | that tree does not explain. That tree is *induced* by the graph whether the
-- | designer looked at it or not, which is why a tool that only ever draws the
-- | graph is showing the designer something other than what the user receives.
-- |
-- | Sorting the leftover edges is where the signal is. A **back** edge is the
-- | way out, and its absence is a trap. A **forward** edge skips levels — a
-- | shortcut, directness bought at the cost of taxonomy. A **cross** edge jumps
-- | into a different subtree, which is the interlevel transition that makes
-- | where-you-are impossible to reconstruct from how-you-got-here.
-- |
-- | ## Why the search is written here
-- |
-- | `Data.Graph.Pathfinding.bfs` looks like exactly this and is not usable for
-- | it: `Data.Graph.Types.buildAdjacency` inserts every edge in *both*
-- | directions, so that graph is undirected. A navigation graph is directed, and
-- | the most useful question about one — "in by one press, out by four" — is
-- | meaningless without direction. A directed BFS returning its search tree is a
-- | real gap in `hylograph-graph`; this is a local stand-in, and a candidate to
-- | move there once a second consumer (Site Explorer's route graph) wants it.
module Glassbox.Tree
  ( EdgeClass(..)
  , edgeClassLabel
  , Induced
  , induce
  , classOf
  , depthOf
  , returnCostOf
  , asymmetries
  , pathFromRoot
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

-- | What an edge is, relative to the tree the user experiences.
data EdgeClass
  = TreeEdge         -- ^ how you first arrive; the spine of the mental model
  | BackEdge         -- ^ to an ancestor: the way out
  | ForwardEdge      -- ^ skips levels toward a descendant: a shortcut
  | CrossEdge        -- ^ into a different subtree: an interlevel jump
  | SelfEdge         -- ^ a self-loop
  | FromUnreachable  -- ^ leaves a state home cannot reach at all

derive instance eqEdgeClass :: Eq EdgeClass
derive instance ordEdgeClass :: Ord EdgeClass

edgeClassLabel :: EdgeClass -> String
edgeClassLabel = case _ of
  TreeEdge -> "tree"
  BackEdge -> "back"
  ForwardEdge -> "forward"
  CrossEdge -> "cross"
  SelfEdge -> "self"
  FromUnreachable -> "unreachable"

type Induced =
  { root :: String
  , depth :: Map String Int          -- ^ fewest steps from home
  , returnCost :: Map String Int     -- ^ fewest steps back to home
  , parent :: Map String String
  , children :: Map String (Array String)
  , classes :: Map (Tuple String String) EdgeClass
  , unreachable :: Array String
  }

-- | Induce the tree, and classify every edge against it.
induce :: String -> Array String -> Array (Tuple String String) -> Induced
induce root allNodes allEdges =
  { root
  , depth: forward.depth
  , returnCost: backward.depth
  , parent: forward.parent
  , children
  , classes: Map.fromFoldable (map (\e -> Tuple e (classify e)) allEdges)
  , unreachable: Array.filter (\n -> not (Map.member n forward.depth)) allNodes
  }
  where
  adjacency = adjacencyOf allEdges
  -- Reversing the edges and searching again gives, for every state, the fewest
  -- steps back to home — which is the other half of "in by one, out by four".
  reversed = adjacencyOf (map (\(Tuple f t) -> Tuple t f) allEdges)

  forward = search root adjacency
  backward = search root reversed

  children = Map.fromFoldableWith (<>) $
    map (\(Tuple child par) -> Tuple par [ child ])
      (Map.toUnfoldable forward.parent :: Array (Tuple String String))

  isAncestorOf ancestor node = go node
    where
    go n = case Map.lookup n forward.parent of
      Nothing -> false
      Just p -> p == ancestor || go p

  classify (Tuple from to)
    | from == to = SelfEdge
    | not (Map.member from forward.depth) = FromUnreachable
    | Map.lookup to forward.parent == Just from = TreeEdge
    | isAncestorOf to from = BackEdge
    | isAncestorOf from to = ForwardEdge
    | otherwise = CrossEdge

adjacencyOf :: Array (Tuple String String) -> Map String (Array String)
adjacencyOf pairs = Map.fromFoldableWith (<>) (map (\(Tuple f t) -> Tuple f [ t ]) pairs)

-- | Directed breadth-first search, keeping the search tree.
search
  :: String
  -> Map String (Array String)
  -> { depth :: Map String Int, parent :: Map String String }
search root adjacency = go { depth: Map.singleton root 0, parent: Map.empty } [ root ]
  where
  go acc queue = case Array.uncons queue of
    Nothing -> acc
    Just { head, tail } ->
      let
        here = fromMaybe 0 (Map.lookup head acc.depth)
        visit st child
          | Map.member child st.acc.depth = st
          | otherwise = st
              { acc =
                  { depth: Map.insert child (here + 1) st.acc.depth
                  , parent: Map.insert child head st.acc.parent
                  }
              , queue = Array.snoc st.queue child
              }
        stepped = Array.foldl visit { acc, queue: tail } (fromMaybe [] (Map.lookup head adjacency))
      in
        go stepped.acc stepped.queue

classOf :: Induced -> String -> String -> Maybe EdgeClass
classOf induced from to = Map.lookup (Tuple from to) induced.classes

depthOf :: Induced -> String -> Maybe Int
depthOf induced node = Map.lookup node induced.depth

returnCostOf :: Induced -> String -> Maybe Int
returnCostOf induced node = Map.lookup node induced.returnCost

-- | States that are much harder to leave than to reach.
-- |
-- | The single most reliable signature of an awful menu: in by one press, out
-- | by four. Reported worst-first, and only where the return actually costs
-- | more than the arrival.
asymmetries :: Induced -> Array { state :: String, inCost :: Int, outCost :: Int, excess :: Int }
asymmetries induced =
  Array.sortBy (\a b -> compare b.excess a.excess) $
    Array.mapMaybe measure (Map.toUnfoldable induced.depth :: Array (Tuple String Int))
  where
  measure (Tuple state inCost) = case Map.lookup state induced.returnCost of
    Just outCost | outCost > inCost ->
      Just { state, inCost, outCost, excess: outCost - inCost }
    _ -> Nothing


-- | How you get here from home, as the tree tells it.
-- |
-- | The user's own question, and the induced tree is exactly the thing that
-- | answers it: walk the parent chain up and reverse. Empty for a state home
-- | cannot reach.
pathFromRoot :: Induced -> String -> Array String
pathFromRoot induced node
  | not (Map.member node induced.depth) = []
  | otherwise = Array.reverse (climb node [ node ])
      where
      climb n acc = case Map.lookup n induced.parent of
        Nothing -> acc
        Just parent -> climb parent (Array.snoc acc parent)
