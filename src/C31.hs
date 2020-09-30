module C31
  (result)
  where

import Data.Array
import Data.List

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

isCoin :: Int -> Bool
isCoin n = n `elem` coins

countCombinations :: Int -> [Int] -> [[Int]]
countCombinations 0 _   = [[]]
countCombinations n []  = []
countCombinations n coins@(c:cs)
  | c <= n    = map (c:) (countCombinations (n-c) coins) ++
                (countCombinations n cs)
  | otherwise = countCombinations n cs

result = countCombinations 200 coins
