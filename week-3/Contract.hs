import Prelude hiding (drop, filter, length)
import Control.Monad (liftM, when, unless)
import Data.Random (runRVar)
import Data.Random.Extras (choiceSeq)
import Data.Random.Source.DevRandom (DevRandom( DevURandom ))
import Data.Sequence (Seq, (><), (<|), drop, filter, fromList, index, length, partition)
import System.IO (hFlush, stdout)
import System.ProgressBar (progressBar, msg, percentage)
import Test.HUnit ((~:), assertEqual, runTestTT, test)

debug = False

type Vertex = Int
type Edge = (Vertex, Vertex)
type Adjacency = (Vertex, Seq Vertex)
type Graph = Seq Adjacency

contract :: Graph -> Edge -> Graph
contract g (v1, v2) = fmap f gWithoutV2
    where
    -- extract the v2 row from the graph
    ((_, v2Adjacents), gWithoutV2) = let (v2Row, gWithoutV2) = partition (\(v, _) -> v == v2) g in (index v2Row 0, gWithoutV2)
    -- add the v2 edges to the v1 row and remove self loops, transform v2 -> v1 in all other rows
    f (v, adjacents)   = (v, if v == v1 then removeSelfLoops (adjacents >< v2Adjacents)
                                        else v2ToV1 adjacents)
    removeSelfLoops as = filter (\a -> a /= v1 && a /= v2) as       -- filter any occurrences of v1 or v2 in the row
    v2ToV1 as          = fmap (\a -> if a == v2 then v1 else a) as  -- transform v2 -> v1 in all other rows

pickEdge :: Graph -> IO Edge
pickEdge g = do
  (v1, v1Adjacents) <- runRVar (choiceSeq g) DevURandom
  v2 <- runRVar (choiceSeq v1Adjacents) DevURandom
  return (v1, v2)

minCut :: Graph -> IO Int
minCut g
  | length g == 2 = return $ length (snd (index g 0)) -- return the number of edges connected to the first vertex
  | otherwise = pickEdge g >>= minCut . (contract g)

manyMinCut :: Int -> Graph -> IO Int
manyMinCut iterations g = go iterations g maxBound
  where
  go :: Int -> Graph -> Int -> IO Int
  go 0 g min = do
    putStrLn ""
    return min
  go n g min = do
    cut <- minCut g
    let newMin = if cut < min then cut else min
    printInfo n cut newMin
    go (n-1) g newMin
  printInfo n cut newMin = do
    when debug $ putStrLn $ "attempts remaining: " ++ show n ++ ", this time: " ++ show cut ++ ", min so far: " ++ show newMin
    unless debug $ progressBar (msg "working") percentage 70 (toInteger iterations - toInteger n) (toInteger iterations)
    hFlush stdout

readGraph :: String -> Graph
readGraph str = fromList $ map (\line ->
                                  let vs = map read (words line) in
                                  (head vs, fromList (tail vs))) (lines str)

main = (liftM readGraph . readFile) "kargerMinCut.txt" >>= manyMinCut 100 >>= putStrLn . show

tg1 = readGraph "1 2 3 4 7\n2 1 3 4\n3 1 2 4\n4 1 2 3 5\n5 4 6 7 8\n6 5 7 8\n7 1 5 6 8\n8 5 6 7"
tg2 = readGraph "1 4 2 7 3\n2 4 1 3\n3 1 2 4\n4 5 1 2 3\n5 8 7 6 4\n6 8 5 7\n7 6 8 5 1\n8 7 6 5"
tg3 = readGraph "1 2 3 4\n2 1 3 4\n3 1 2 4\n4 1 2 3 5\n5 4 6 7 8\n6 5 7 8\n7 5 6 8\n8 5 6 7"
tg4 = readGraph "1 3 4 2\n2 1 4 3\n3 1 2 4\n4 5 3 2 1\n5 4 8 6 7\n6 8 7 5\n7 5 8 6\n8 5 7 6"
tg5 = readGraph "1 19 15 36 23 18 39 \n2 36 23 4 18 26 9\n3 35 6 16 11\n4 23 2 18 24\n5 14 8 29 21\n6 34 35 3 16\n7 30 33 38 28\n8 12 14 5 29 31\n9 39 13 20 10 17 2\n10 9 20 12 14 29\n11 3 16 30 33 26\n12 20 10 14 8\n13 24 39 9 20\n14 10 12 8 5\n15 26 19 1 36\n16 6 3 11 30 17 35 32\n17 38 28 32 40 9 16\n18 2 4 24 39 1\n19 27 26 15 1\n20 13 9 10 12\n21 5 29 25 37\n22 32 40 34 35\n23 1 36 2 4\n24 4 18 39 13\n25 29 21 37 31\n26 31 27 19 15 11 2\n27 37 31 26 19 29\n28 7 38 17 32\n29 8 5 21 25 10 27\n30 16 11 33 7 37\n31 25 37 27 26 8\n32 28 17 40 22 16\n33 11 30 7 38\n34 40 22 35 6\n35 22 34 6 3 16\n36 15 1 23 2\n37 21 25 31 27 30\n38 33 7 28 17 40\n39 18 24 13 9 1\n40 17 32 22 34 38"

tests = test [ "test1" ~: manyMinCut 1000 tg1 >>= assertEqual "test1" 2,
               "test2" ~: manyMinCut 1000 tg2 >>= assertEqual "test2" 2,
               "test3" ~: manyMinCut 1000 tg3 >>= assertEqual "test3" 1,
               "test4" ~: manyMinCut 1000 tg4 >>= assertEqual "test4" 1,
               "test5" ~: manyMinCut 1000 tg5 >>= assertEqual "test5" 3
             ]
