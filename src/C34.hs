module C34
  (result, digits)
  where

import Data.Map (fromList, (!), Map)

factorials :: Map Int Int
factorials = fromList (zip [0..] ([1] ++ (scanl1 (*) [1..9])))

limit = sum factorials

possible = takeWhile (<limit) [1..]

digits :: Int -> [Int]
digits n
  | x == 0      = [y]
  | otherwise   = y:digits x
  where (x, y) = quotRem n 10

curious :: Int -> Bool
curious n
  | n < 3     = False
  | otherwise = sum (map  (\x -> factorials ! x) (digits n)) == n

result = sum $ filter curious possible
