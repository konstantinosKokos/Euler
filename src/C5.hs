module C5
    ( result
    ) where

import Data.Sort (sortOn)
import C3 (primeFactors)

cands = [1..20]
all_divisors = concat [primeFactors x | x <- cands]
sorted_divisors = reverse $ sortOn (\x -> length (filter (\y -> y == x) all_divisors)) cands

result = head $ filter (\x -> all (\y -> x `mod` y == 0) sorted_divisors) [1..]
