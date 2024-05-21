
module Main where

import Route
--import RouteGUI           -- Leave this un
import qualified Graph as G -- Create a module and use a sensible graph representation
import Data.PSQueue qualified as Q
import Data.Map qualified as M
import System.Environment ( getArgs )
import Data.Maybe

shortestPath :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to
  | not (G.member from g) = Nothing                     -- Return Nothing if start node dosen't exist in the graph,
  | not (G.member to g)   = Nothing                     -- return Nothing if end node dosen't exist in the graph,
  | otherwise             = Just (path, weight)         -- otherwise return the tuple consisting of the path and the total cost.
  where
    map            = dijkstra g from
    (path, weight) = extractPathAndWeight map from to
  
  
extractPathAndWeight :: (Ord a, Ord b, Num b) => M.Map a (b, a) -> a -> a -> ([a], b)
extractPathAndWeight distMap start end = case M.lookup end distMap of
  Just (totalWeight, _)               -> (reverse (buildPath end), totalWeight)
  Nothing                             -> ([], 0)
  where
  buildPath node
    | node == start = [start]
    | otherwise = node : buildPath (snd (fromJust (M.lookup node distMap)))


dijkstra :: (Ord a, Ord b, Num b) => G.Graph a b -> a -> M.Map a (b, a)
dijkstra g from
  -- If the start node dosent exist in the graph we call the help function by adding the first node to the distance map and the queue
  | G.member from g = dijkstra' g (M.singleton from (0, from)) (Q.singleton from 0)
  | otherwise       = error "starting node is not in the input graph"


dijkstra' :: (Ord a, Ord b, Num b) => G.Graph a b -> M.Map a (b, a) -> Q.PSQ a b -> M.Map a (b, a)
dijkstra' g dmap pq
  | Q.null pq  = dmap                      -- When the queue is empty function returns the new map,
  | otherwise  = dijkstra' g dmap' pq'     -- otherwise we recursivly call the dijkstra' func with updated pq and map.
  where
  (next Q.:-> currentDist, pq') = fromJust (Q.minView pq)
  adjList = G.adj next g
  (dmap', pq'') = foldr (update currentDist next) (dmap, pq') adjList


update :: (Ord a, Ord b, Num b) => b -> a -> G.Edge a b -> (M.Map a (b, a), Q.PSQ a b) -> (M.Map a (b, a), Q.PSQ a b)
update currentDist next (G.Edge _ neighbor weight) (dmap, pq) =
  let newDist = currentDist + weight
  in case M.lookup neighbor dmap of
    Just (oldDist, _)
      | newDist < oldDist -> (M.insert neighbor (newDist, next) dmap, Q.insert neighbor newDist pq)
      | otherwise         -> (dmap, pq)
    Nothing               -> (M.insert neighbor (newDist, next) dmap, Q.insert neighbor newDist pq)


-- Building a graph with stops and lines
buildGraph :: [Stop] -> [LineTable] -> G.Graph String Integer
buildGraph stops = foldr addLineEdges G.empty
  where
    addLineEdges (LineTable _ stops) g = foldr addStopEdge g (zip stops (tail stops))
    addStopEdge (LineStop s1 t1, LineStop s2 t2) g = G.addEdge s1 s2 t2 (G.addEdge s2 s1 t2 g)
-- Fråga Chat ig

 -- TODO: read arguments, build graph, output shortest path
main :: IO ()
main = do

  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  putStrLn "Ange start hållplats"
  from <- getLine
  putStrLn "Ange Destination"
  to <- getLine
      
  let graph = buildGraph stops lines
  case shortestPath graph from to of
    Just (path, label) -> putStrLn $ "Shortest path: " ++ unwords path ++ " with label: " ++ show label
    Nothing -> putStrLn "No path found."
    --Nothing -> putStrLn "Incorrect input or something idk" Kommenterade ut detta för tillfället för det fkin bråkar

{-
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath
-}




