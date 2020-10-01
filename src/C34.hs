module C34
  (result)
  where

import Data.Map (fromList, (!), Map)

factorials :: Map Int Int
factorials = fromList (zip [0..] ([1] ++ (scanl1 (*) [1..9])))

limit = sum factorials

possible = takeWhile (<limit) [1..]

get_digs :: Int -> [Int]
get_digs n
  | x == 0      = [y]
  | otherwise   = y:get_digs x
  where (x, y) = quotRem n 10

curious :: Int -> Bool
curious n
  | n < 3     = False
  | otherwise = sum (map  (\x -> factorials ! x) (get_digs n)) == n

result = sum $ filter curious possible
