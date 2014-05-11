-- Answer: 2407905288

import Control.Monad
import Test.HUnit

main = do
  input <- (liftM lines . readFile) "IntegerArray.txt"
  let inputInt = map read input :: [Int]
  putStrLn $ show $ countInversions inputInt

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

tests = test [ "test1" ~: 0  ~=? (countInversions [1,2,3]),
               "test2" ~: 1  ~=? (countInversions [1,3,2]),
               "test3" ~: 3  ~=? (countInversions [1,3,5,2,4,6]),
               "test4" ~: 4  ~=? (countInversions [1,5,3,2,4]),
               "test5" ~: 10 ~=? (countInversions [5,4,3,2,1]),
               "test6" ~: 5  ~=? (countInversions [1,6,3,2,4,5]),
               "test7" ~: 0  ~=? (countInversions ["1000", "11"]) -- watch out for string inputs!
             ]
