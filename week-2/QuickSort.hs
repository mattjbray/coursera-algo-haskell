import Control.Monad.Writer
import qualified Data.Vector as V
import Test.HUnit

main = do
  let (sorted, swapCount) = runQuicksort $ V.fromList [3, 9, 8, 4, 6, 10, 2, 5, 7, 1]
  putStrLn $ "sorted: " ++ (show $ V.toList $ sorted)
  putStrLn $ "swaps: " ++ (show $ getSum $ swapCount)

runQuicksort :: Ord a => V.Vector a -> (V.Vector a, Sum Int)
runQuicksort xs = runWriter $ quicksort xs

quicksort :: Ord a => V.Vector a -> Writer (Sum Int) (V.Vector a)
quicksort xs
  | V.length xs <= 1 = return $ xs
  | otherwise = do
                  let pivotIndex = choosePivot xs
                  partitionedXs <- partition pivotIndex xs
                  let pivot = partitionedXs V.! pivotIndex
                  left  <- quicksort $ V.take pivotIndex partitionedXs
                  right <- quicksort $ V.drop (pivotIndex+1) partitionedXs
                  return $ left V.++ (V.singleton pivot) V.++ right

choosePivot :: V.Vector a -> Int
choosePivot xs = 0

partition :: Ord a => Int -> V.Vector a -> Writer (Sum Int) (V.Vector a)
partition 0 xs = go 1 1 xs
  where
  go i j xs
    | j == V.length xs = swap 0 (i-1) xs --(left, right)
    | (xs V.! j) > p   = go i (j+1) xs
    | otherwise        = do
        swapped <- swap i j xs
        go (i+1) (j+1) swapped
      where
      p = V.head xs
      left = V.slice 1 (i-1) xs
      right = V.slice i (j-i) xs
-- if the pivot is not the first element, move it to the front
partition pivotIndex xs = do
  swapped <- swap 0 pivotIndex xs
  partition 0 swapped

swap :: Int -> Int -> V.Vector a -> Writer (Sum Int) (V.Vector a)
swap i j xs = do
  tell (Sum 1)
  return $ V.update xs (V.fromList [(i, xs V.! j), (j, xs V.! i)])

-- tests = test [ "test partition" ~: (V.fromList [1,2,3,5,8,4,7,6]) ~=? (partition 0 (V.fromList [3,8,2,5,1,4,7,6])),
--                "test quicksort" ~: (V.fromList [1,2,3,4,5,6,7,8]) ~=? (quicksort (V.fromList [3,8,2,5,1,4,7,6])),
--                "test swap"      ~: (V.fromList [0,1,2,5,4,3,6,7,8]) ~=? (swap 3 5 (V.fromList [0,1,2,3,4,5,6,7,8]))
--              ]
