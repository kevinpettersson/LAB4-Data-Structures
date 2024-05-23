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
  | otherwise             = Just (reverse path, weight)     -- Return the path and the total weight of the path taken, reversing the path so its the order of the invariant.
  where
    weight = label (fromJust (Q.lookup to pq))              -- get the wheight by retriving the edge from the dst node
    pq     = dijkstra g from                                -- our final vistied pq we get from calling the algorithm
    path   = getPath pq from to                             -- turns the visited pq into a list representing the path taken.

-- getPath returns the path from source to destination
getPath :: (Ord a, Ord b, Num b) => Q.PSQ a (Edge a b) -> a -> a -> [a]
getPath pq from to
  | isNothing elem = []
  | from /= to     = to : getPath pq from previousNode                  -- prepending the next node and recursivly calling the func
  | otherwise      = [to]
  where
    elem         = Q.lookup to pq                                       -- retrvies the edge of the dst node
    previousNode = src (fromJust elem)                                  -- retrives the privous node by looking up the src of the edge

-- initiate the recursive helper function if the src node exists in the graph. 
dijkstra :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> Q.PSQ a (Edge a b)
dijkstra g from
  | G.member from g = dijkstra' g (Q.singleton from (Edge from from 0)) Q.empty            -- initiate our algorithm by passing the work-pq with first node and an empty vistied-pq
  | otherwise       = Q.empty                                                              -- return empty pq

-- when pq is empty we return the pq with visited nodes. 
dijkstra' :: (Ord a, Ord b, Num b) => G.Graph a b -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b)
dijkstra' g queue visited
  | Q.null queue = visited                                                                  -- queue is empty we the algorithm is done and we return the queue holding the path.
  | otherwise    = dijkstra' g newQueue newVisited                                          -- recursive call of djikstra' with the updated work-pq and visited-pq
  where
    (currentNode, updatedQueue) = getMinNode queue                                          -- retrieves the node with min priority and the updated queue
    newVisited                  = Q.insert (Q.key currentNode) (Q.prio currentNode) visited -- insert the next node and edge to the visited-pq 
    newQueue                    = addNeighbors g updatedQueue newVisited currentNode        -- retrieves the new queue by adding neigbours of new node.

-- get the node with the minimum weight
getMinNode :: (Ord a, Ord b) => Q.PSQ a (Edge a b) -> (Q.Binding a (Edge a b), Q.PSQ a (Edge a b))
getMinNode queue = (fromJust (Q.findMin queue), Q.deleteMin queue)            -- returns the node with the minimum weight

-- add or update the neighbors of the current node in the queue
addNeighbors :: (Ord a, Ord b, Num b) => G.Graph a b -> Q.PSQ a (Edge a b) -> Q.PSQ a (Edge a b) -> Q.Binding a (Edge a b) -> Q.PSQ a (Edge a b)
addNeighbors graph queue visited currentNode = 
    foldl (addNeighbor visited currentEdge) queue (G.adj currentName graph)   -- updates the queue with the shortesht path to each neighbour 
  where
    currentName = Q.key currentNode
    currentEdge = Q.prio currentNode

-- add or update a neighbor in the queue if a shorter path is found
addNeighbor :: (Ord a, Ord b, Num b) => Q.PSQ a (Edge a b) -> Edge a b -> Q.PSQ a (Edge a b) -> Edge a b -> Q.PSQ a (Edge a b)
addNeighbor visited currentEdge queue edge
  | isJust (Q.lookup neighbor visited)                        = queue                           -- dst is already visited return queue
  | isNothing oldEdge || newWeight < label (fromJust oldEdge) = Q.insert neighbor newEdge queue -- insert old edge if new edge cost less
  | otherwise                                                 = queue
  where
    neighbor  = dst edge                                                    -- retrive the dst of a edge
    newWeight = label edge + label currentEdge                              -- adding the veight of current edge and neighbour
    oldEdge   = Q.lookup neighbor queue                                     -- retrive the edge of neighbour
    newEdge   = Edge (src edge) neighbor newWeight                          -- creates a new edge by taking source of edge, dst = neighbour and label as newwieght

-- build the graph from stops and line tables
buildGraph :: [Stop] -> [LineTable] -> G.Graph String Integer
buildGraph stops = foldr addEdges initialGraph
  where
    initialGraph                                  =             -- instaniate the graph by adding all stops 
      foldr (G.addVertex . name) G.empty stops   

    addEdges (LineTable _ stops) graph            =             -- each line table represents a sequence of stops and time.
      foldr addStopEdge graph (zip stops (drop 1 stops))

    addStopEdge (LineStop s1 _, LineStop s2 t2) g =             -- s1 and s2 are the names of the stops, and t2 is the travel time between them.
      G.addEdge s1 s2 t2  g

main :: IO ()
main = do
  args <- getArgs 
  Right stops <- readStops (head args)
  Right lines <- readLines (args !! 1)
  let
    from  = args !! 2
    to    = args !! 3
    graph = buildGraph stops lines          -- graph made by calling buildGraph
    path  = shortestPath graph from to      -- result tuple containg a list (path) and a weight (cost / time)
  case path of
    Just (pathList, weight) -> do
      print weight
      putStr (unlines pathList)
    Nothing -> putStrLn "Path doesn't exist"