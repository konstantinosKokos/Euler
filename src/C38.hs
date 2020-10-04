module C38
    (result
    ) where

import C32 (isPandigit)
import Data.Char (digitToInt)
import Data.List (sort)

cProd :: Int -> Int -> Int -> [Int]
cProd f left num
  | left - (length ds) <= 0 = ds
  | otherwise               = ds ++ (cProd (f+1) (left - (length ds)) (num))
  where
    ds = map digitToInt $ show (f*num)

result = maximum $
  map (foldl (\x y -> 10 * x + y) 0) $
  filter (isPandigit [1..9]) (map (cProd 1 9) [1..9999])
