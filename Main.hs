{-# LANGUAGE NondecreasingIndentation #-}
module Main where

import System.Environment (getArgs)
import Data.Maybe ( fromJust, isNothing, isJust )
import Data.PSQueue qualified as Q
import Graph qualified as G
import Graph (Edge(..))
import Route

-- Returns the fastest path between two nodes.
shortestPath :: (Ord node, Ord weight, Num weight) => G.Graph node weight -> node -> node -> Maybe ([node], weight)
shortestPath g from to
  | not (G.member to g)   = Nothing
  | not (G.member from g) = Nothing
  | null path             = Just ([], 2147483647)           -- For some reason the nopath test want a empty list and 2147483647
  | otherwise             = Just (reverse path, weight)     -- Return the path and the total weight of the path taken.
  where
    weight = label (fromJust (Q.lookup to pq))
    pq     = dijkstra g from
    path   = getPath pq from to

-- getPath returns the path from source to destination by backtracking through the priority queue.
getPath :: (Ord a, Ord b, Num b) => Q.PSQ a (Edge a b) -> a -> a -> [a]
getPath pq from to
  | isNothing elem = []
  | from /= to     = to : getPath pq from previousNode
  | otherwise      = [to]
  where
    elem         = Q.lookup to pq
    previousNode = src (fromJust elem)

-- Dijkstra's algorithm: initiate the recursive helper function if the src node exists in the graph. 
dijkstra :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> Q.PSQ a (Edge a b)
dijkstra g from
  | G.member from g = dijkstra' g (Q.singleton from (Edge from from 0)) Q.empty
  | otherwise       = Q.empty

-- If pq is empty we return the pq with visited nodes. 
dijkstra' :: (Ord a, Ord b, Num b) => G.Graph a b -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b)
dijkstra' g queue visited
  | Q.null queue = visited
  | otherwise    = dijkstra' g newQueue newVisited
  where
    (currentNode, updatedQueue) = getMinNode queue
    newVisited                  = Q.insert (Q.key currentNode) (Q.prio currentNode) visited
    newQueue                    = addNeighbors g updatedQueue newVisited currentNode

-- Get the node with the minimum weight
getMinNode :: (Ord a, Ord b) => Q.PSQ a (Edge a b) -> (Q.Binding a (Edge a b), Q.PSQ a (Edge a b))
getMinNode queue = (fromJust (Q.findMin queue), Q.deleteMin queue)

-- Add or update the neighbors of the current node in the queue
addNeighbors :: (Ord a, Ord b, Num b) => G.Graph a b -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b) -> Q.Binding a (Edge a b) -> Q.PSQ a (Edge a b)
addNeighbors graph queue visited currentNode = 
    foldl (addNeighbor visited currentEdge) queue (G.adj currentName graph)
  where
    currentName = Q.key currentNode
    currentEdge = Q.prio currentNode

-- Add or update a neighbor in the queue if a shorter path is found
addNeighbor :: (Ord a, Ord b, Num b) => Q.PSQ a (Edge a b) -> Edge a b -> Q.PSQ a (Edge a b) -> Edge a b -> Q.PSQ a (Edge a b)
addNeighbor visited currentEdge queue edge
  | isJust (Q.lookup neighbor visited)                        = queue
  | isNothing oldEdge || newWeight < label (fromJust oldEdge) = Q.insert neighbor newEdge queue
  | otherwise                                                 = queue
  where
    neighbor  = dst edge
    newWeight = label edge + label currentEdge
    oldEdge   = Q.lookup neighbor queue
    newEdge   = Edge (src edge) neighbor newWeight

-- Build the graph from stops and line tables
buildGraph :: [Stop] -> [LineTable] -> G.Graph String Integer
buildGraph stops = foldr addEdges initialGraph
  where
    initialGraph                                  = foldr (G.addVertex . name) G.empty stops
    addEdges (LineTable _ stops) graph            = foldr addStopEdge graph (zip stops (drop 1 stops))
    addStopEdge (LineStop s1 _, LineStop s2 t2) g = G.addEdge s1 s2 t2 (G.addEdge s2 s1 t2 g)

main :: IO ()
main = do
  args <- getArgs
  Right stops <- readStops (head args)
  Right lines <- readLines (args !! 1)
  let
    from  = args !! 2
    to    = args !! 3
    graph = buildGraph stops lines
    path  = shortestPath graph from to
  case path of
    Just (pathList, weight) -> do
      print weight
      putStr (unlines pathList)
    Nothing -> putStrLn "Path doesn't exist"