main = do
  putStrLn $ show $ mergeSort [4, 3, 1, 7, 6, 3, 2]

mergeSort :: Ord a
  => [a] -- unsorted input list (xys)
  -> [a] -- sorted output
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xys = merge (mergeSort xs) (mergeSort ys)
  where
  (xs, ys) = splitAt (length xys `div` 2) xys

merge :: Ord a
  => [a] -- sorted list to be merged (xs)
  -> [a] -- sorted list to be merged (ys)
  -> [a] -- sorted merged list
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | (x < y)   = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
