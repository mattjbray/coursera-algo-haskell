import Control.Monad

main = do
  input <- (liftM lines . readFile) "IntegerArray.txt"
  putStrLn $ show $ countInversions input

countInversions :: Ord a
  => [a]
  -> Int
countInversions xs = fst $ go (0, xs)
  where
  go :: Ord a => (Int, [a]) -> (Int, [a])
  go (count, []) = (count, [])
  go (count, [x]) = (count, [x])
  go (count, yzs) = countAndMerge (count, []) (go (0, ys)) (go (0, zs))
    where
    (ys, zs) = splitAt (length yzs `div` 2) yzs

countAndMerge :: Ord a
  => (Int, [a]) -- accumulator
  -> (Int, [a]) -- input 1
  -> (Int, [a]) -- input 2
  -> (Int, [a]) -- result
countAndMerge (count, res) (xCount, []) (yCount, ys) = (xCount + yCount + count, res ++ ys)
countAndMerge (count, res) (xCount, xs) (yCount, []) = (xCount + yCount + count, res ++ xs)
countAndMerge (count, res) (xCount, (x:xs)) (yCount, (y:ys))
  | (x < y)   = countAndMerge (count, res ++ [x]) (xCount, xs) (yCount, (y:ys))
  | otherwise = countAndMerge (count + (length (x:xs)), res ++ [y]) (xCount, (x:xs)) (yCount, ys)
