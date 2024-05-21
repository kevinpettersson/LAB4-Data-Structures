module Graph
  ( -- * Edge
    Edge(..)                    -- type
  , src, dst, label         -- querying an Edge

    -- * Graph
  , Graph                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges         -- querying a Graph
  , member,          
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
--import Graphics.UI.Threepenny (underline)

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source node (vertex) 
  , dst   :: a  -- ^ Destination node (vertex)
  , label :: b  -- ^ The label (weight)
  } deriving Show

-- TODO: implement a graph with adjacency lists, hint: use a Map.
-- A graph with nodes of type a and labels of type b.
-- A Graph is parametrized on two types, consist of an Map that maps nodes of type a togheter with a list of edges.
data Graph a b = Graph { adjencyList :: Map a [Edge a b] } deriving Show

-- Returns a Graph with an empty Map using Map.empty function.
-- | Complexity: O(1)
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
-- Function checks so the node dosent already exist in the map and then inserts the node togheter with an empty list into the graph.
-- | Complexity: O(log n)
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g
  | M.notMember v (adjencyList g) = g { adjencyList = M.insert v [] (adjencyList g) }
  | otherwise                     = g

-- | Add a list of vertices to a graph
-- Checks each element in the list if it's key already exist, if not we recursivly call the function with the new resulted graph.
-- | Complexity: O(log n)
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices [] g                  = g
addVertices (v:vs) g
  | M.notMember v (adjencyList g) = addVertices vs (addVertex v g)
  | otherwise                     = addVertices vs g

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
-- Function uses record syntax and adjust the the edge list of node v by using the cons operator with the new edge.
-- | Complexity: O(log n)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l g = g {adjencyList = M.adjust (edge :) v (adjencyList g)}
  where
    edge        = Edge {src = v, dst = w, label = l}

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
-- The function calls the addEdge function to add an edge then calls the addEdge function again on  the result of the first call.
-- Since the addEdge function returns the new graph, all that is neeed is switch the order of the given dst and src nodes on the second call.
-- | Complexity: O(log n)
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l g = addEdge w v l (addEdge v w l g) 

-- | Get all adjacent vertices (nodes) for a given node
-- | Complexity: O(log n)
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v g = M.findWithDefault [] v (adjencyList g)

-- | Get all vertices (nodes) in a graph
-- | Complexity: O(n)
vertices :: Graph a b -> [a]
vertices g = M.keys (adjencyList g)

-- | Get all edges in a graph
-- | Complexity: O(2n)  Inte helt säker //OttoPotto
edges :: Graph a b -> [Edge a b]
edges g = concat (M.elems (adjencyList g))

-- Check if vertex exists in map
member :: Ord a => a -> Graph a b -> Bool
member v (Graph map) = M.member v map

