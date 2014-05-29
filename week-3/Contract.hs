import Data.Random (runRVar)
import Data.Random.Extras (choices)
import Data.Random.Source.DevRandom (DevRandom( DevURandom ))

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [[Vertex]]

testGraph :: Graph
testGraph = [
    [0, 1, 2],
    [1, 0, 2, 3],
    [2, 1, 0, 3],
    [3, 1, 2]
  ]

contract :: Edge -> Graph -> Graph
contract (v1, v2) g
  | v1 > v2 = contract (v2, v1) g                             -- ensure v2 > v1
  | otherwise = map f [r | r <- g, head r /= v2]              -- filter out the v2 row and map f over the result
    where
    v2Edges = head $ [r | r <- g, head r == v2]               -- the edges for v2
    f row   = if head row == v1
              then removeSelfLoops (row ++ (tail v2Edges))    -- add the v2 edges to the v1 row and remove self loops
              else map (\v -> if v == v2 then v1 else v) row  -- transform v2 -> v1 in all other rows
    removeSelfLoops (v:row) = v : [vertex | vertex <- row, vertex /= v1 && vertex /= v2]  -- filter any occurrences of v1 or v2 in the row

pickEdge :: Graph -> IO Edge
pickEdge g = do
  let vertices = [head r | r <- g]
  [v1, v2] <- runRVar (choices 2 vertices) DevURandom
  return (v1, v2)

minCut :: Graph -> IO Int
minCut [r1, r2] = return $ length r1 - 1
minCut g = do
  edge <- pickEdge g
  minCut $ contract edge g

manyMinCut :: Int -> Graph -> IO Int
manyMinCut n g = go n g 999999999
  where
  go :: Int -> Graph -> Int -> IO Int
  go 0 g min = return min
  go n g min = do
    cut <- minCut g
    putStrLn $ "this time: " ++ show cut ++ ", min so far: " ++ show min
    go (n-1) g (if cut < min then cut else min)

main = do
  input <- readFile "kargerMinCut.txt"
  let graph = map (map read . words) (lines input) :: Graph
  result <- manyMinCut 1000 graph
  putStrLn $ show $ result
