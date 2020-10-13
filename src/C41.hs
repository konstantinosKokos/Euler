module C41
    (
    permute', result
    ) where

import C27 (isPrime)
import Data.List (delete)
import Data.Char (digitToInt, intToDigit)

permute :: Ord a => [a] -> [[a]]
permute []      = [[]]
permute ds  = [(n:ns) | n <- ds, ns <- permute (delete n ds)]

permute' :: Ord a => [a] -> [[a]]
permute' []       = []
permute' (d:ds)   = permute (d:ds) ++ permute' ds

eval :: [Int] -> Int
eval xs = read (map intToDigit xs)

result = maximum $
          filter isPrime
          (map eval (permute' (reverse [1..9])))
