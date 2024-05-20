
module Main where
    
import Route
import RouteGUI
import Graph -- Create a module and use a sensible graph representation
--import Data.PSQueue qualified as Q
import AATree qualified as Q
import Data.Map qualified as M
import AATree (AATree)

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined                                      

dijkstra :: Graph a b -> a -> Map a (b, a)
dijkstra g from 
  | G.member g from = dijkstra' from g (M.singleton from (0, from)) (Q.singleton from)
  | otherwise       = error "finns ingen väg"

dijkstra' :: a -> Graph a b -> Map a (b, a) -> AATree a -> Map a (b, a)
dijkstra' from g mapp pq
  | Q.empty == pq    = mapp
  | M.notMember from = undefined
  where 
    adjList = adj from g

main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path


startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath



