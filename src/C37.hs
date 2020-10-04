module C37
  (result)
  where

import C3 (primes)
import C27 (isPrime)
import C34 (digits)
import C35 (evaluate)
import Data.List (nub)

isPrime' :: Int -> Bool
isPrime' 1 = False
isPrime' n = isPrime n

trun :: Int -> [Int]
trun n = map evaluate truncations
  where
    ds          = reverse $ digits n
    left        = map (\x -> take x ds) [1..(length ds)]
    right       = map (\x -> reverse $ take x (reverse ds)) [1..length ds]
    truncations = nub $ left ++ right

truncatablePrime :: Int -> Bool
truncatablePrime n = all isPrime' $ trun n

result = take 11 $ filter truncatablePrime (drop 4 primes)
