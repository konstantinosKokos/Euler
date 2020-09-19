module C14
  (result)
  where

import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

m = 1000000

collatzArray :: Array Int Int
collatzArray = listArray (1, m) $ map collatzLength [1..m]

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength n = case inRange (bounds collatzArray) next of
  True  -> 1 + collatzArray ! next
  False -> 1 + collatzLength next
  where
    next = case (even n) of
      True  -> n `div` 2
      False -> 3*n + 1

zs = zip [1..] (map collatzLength [1..m])
result = fst (maximumBy (comparing snd) zs)
