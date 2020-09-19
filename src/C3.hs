module C3
    ( result, primes, factor, primeFactors
    ) where

import Data.List.Ordered (minus, union, unionAll)

primes :: [Int]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

factor :: [Int] -> Int -> [Int]
factor [] n = []
factor xs@(p:ps) n
    | p^2 > n    = [n]
    |  r == 0    = p : factor xs d
    | otherwise  = factor ps n
    where (d, r) = n `divMod` p

primeFactors :: Int -> [Int]
primeFactors n = factor primes n

result = last $ factor primes 600851475143
