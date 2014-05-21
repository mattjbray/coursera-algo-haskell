import Control.Monad
import Control.Monad.Writer
import qualified Data.Vector as V
import qualified Data.List as L
import Test.HUnit

main = do
  sortFile chooseFirst "10.txt"
  sortFile chooseLast "10.txt"
  sortFile chooseFirst "100.txt"
  sortFile chooseLast "100.txt"
  sortFile chooseFirst "1000.txt"
  sortFile chooseLast "1000.txt"

type PivotChooser a = V.Vector a -> (a, Int)

chooseFirst :: PivotChooser Int
chooseFirst xs = (V.head xs, 0)

chooseLast :: PivotChooser Int
chooseLast xs = (V.last xs, V.length xs - 1)

sortFile :: PivotChooser Int -> String -> IO ()
sortFile pivotChooser fileName = do
  input <- (liftM lines . readFile) fileName
  let inputInt = map read input :: [Int]
  let (sorted, compCount) = runQuicksort pivotChooser $ V.fromList inputInt
  putStrLn $ "sorted: " ++ (show $ (L.sort inputInt) == (V.toList sorted))
  putStrLn $ "comparisons: " ++ (show $ getSum $ compCount)

runQuicksort :: Ord a => PivotChooser a -> V.Vector a -> (V.Vector a, Sum Int)
runQuicksort pivotChooser xs = runWriter $ quicksort pivotChooser xs

quicksort :: Ord a => PivotChooser a -> V.Vector a -> Writer (Sum Int) (V.Vector a)
quicksort pivotChooser xs
  | V.length xs <= 1 = return $ xs
  | otherwise = do
                  tell (Sum (V.length xs - 1))
                  let (pivot, pivotIndex) = pivotChooser xs
                  let (left, right) = partition pivotIndex xs
                  leftSorted  <- quicksort pivotChooser left
                  rightSorted <- quicksort pivotChooser right
                  return $ leftSorted V.++ (V.singleton pivot) V.++ rightSorted

partition :: Ord a => Int -> V.Vector a -> (V.Vector a, V.Vector a)
partition 0 xs = go 1 1 xs
  where
  go i j xs
    | j == V.length xs = (left, right)
    | (xs V.! j) > V.head xs = go i (j+1) xs
    | otherwise              = go (i+1) (j+1) $ swap i j xs
      where
      left = V.slice 1 (i-1) xs
      right = V.slice (i) (j-i) xs
-- if the pivot is not the first element, move it to the front
partition pivotIndex xs = partition 0 $ swap 0 pivotIndex xs

swap :: Int -> Int -> V.Vector a -> V.Vector a
swap i j xs = V.update xs (V.fromList [(i, xs V.! j), (j, xs V.! i)])

-- tests = test [ "test partition" ~: (V.fromList [1,2,3,5,8,4,7,6]) ~=? (partition 0 (V.fromList [3,8,2,5,1,4,7,6])),
--                "test quicksort" ~: (V.fromList [1,2,3,4,5,6,7,8]) ~=? (quicksort (V.fromList [3,8,2,5,1,4,7,6])),
--                "test swap"      ~: (V.fromList [0,1,2,5,4,3,6,7,8]) ~=? (swap 3 5 (V.fromList [0,1,2,3,4,5,6,7,8]))
--              ]
