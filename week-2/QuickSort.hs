import qualified Data.Vector as V
import Test.HUnit

main = do
  putStrLn $ show $ V.toList $ quicksort $ V.fromList [1, 3, 2, 4]

quicksort :: Ord a => V.Vector a -> V.Vector a
quicksort xs
  | V.length xs <= 1 = xs
  | otherwise = (quicksort left) V.++ (V.singleton pivot) V.++ (quicksort right)
    where
    pivotIndex = choosePivot xs
    partitionedXs = partition pivotIndex xs
    pivot = partitionedXs V.! pivotIndex
    left = V.take pivotIndex partitionedXs
    right = V.drop (pivotIndex+1) partitionedXs

choosePivot :: V.Vector a -> Int
choosePivot xs = 0

partition :: Ord a => Int -> V.Vector a -> V.Vector a
partition 0 xs = go 1 1 xs
  where
  go i j xs
    | j == V.length xs = swap 0 (i-1) xs --(left, right)
    | (xs V.! j) > p     = go i (j+1) xs
    | otherwise        = go (i+1) (j+1) (swap i j xs)
      where
      p = V.head xs
      left = V.slice 1 (i-1) xs
      right = V.slice i (j-i) xs
-- if the pivot is not the first element, move it to the front
partition pivotIndex xs = partition 0 $ swap 0 pivotIndex xs

swap :: Int -> Int -> V.Vector a -> V.Vector a
swap i j xs = V.update xs (V.fromList [(i, xs V.! j), (j, xs V.! i)])

tests = test [ "test partition" ~: (V.fromList [1,2,3,5,8,4,7,6]) ~=? (partition 0 (V.fromList [3,8,2,5,1,4,7,6])),
               "test quicksort" ~: (V.fromList [1,2,3,4,5,6,7,8]) ~=? (quicksort (V.fromList [3,8,2,5,1,4,7,6])),
               "test swap"      ~: (V.fromList [0,1,2,5,4,3,6,7,8]) ~=? (swap 3 5 (V.fromList [0,1,2,3,4,5,6,7,8]))
             ]
