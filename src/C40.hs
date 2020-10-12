module C40
    (
    result, gen
    ) where

import Data.Char (digitToInt)

gen :: [Int]
gen = map digitToInt $ foldr1 (++) $ map show [0..]

result = foldr1 (*) $
  map (gen!!) [1, 10, 100, 1000, 10000, 100000, 1000000]
