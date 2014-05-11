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
  go (count, yzs) = countAndMerge (go (0, ys)) (go (0, zs))
    where
    (ys, zs) = splitAt (length yzs `div` 2) yzs

countAndMerge :: Ord a
  => (Int, [a]) -- input 1
  -> (Int, [a]) -- input 2
  -> (Int, [a]) -- result
countAndMerge (xCount, []) (yCount, ys) = (xCount + yCount, ys)
countAndMerge (xCount, xs) (yCount, []) = (xCount + yCount, xs)
countAndMerge (xCount, (x:xs)) (yCount, (y:ys))
  | (x < y)   = (count1, x:res1)
  | otherwise = (count2 + (length (x:xs)), y:res2)
  where
  (count1, res1) = countAndMerge (xCount, xs) (yCount, y:ys)
  (count2, res2) = countAndMerge (xCount, x:xs) (yCount, ys)
