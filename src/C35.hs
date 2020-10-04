module C35
  (result, evaluate)
  where

import C34 (digits)
import C3 (primes)
import C27 (isPrime)
import Data.List.Ordered (nub, sort, isect)

primesTo n = takeWhile (<n) primes

evaluate :: [Int] -> Int
evaluate ds = foldl (\x y -> x*10 +y) 0 ds

rotate :: [a] -> [a]
rotate []     = []
rotate [d]    = [d]
rotate (d:ds) = ds ++ [d]

rotations :: [a] -> [[a]]
rotations ds  = take (length ds) $ iterate rotate ds

getRotations :: Int -> [Int]
getRotations n = nub $ sort $ map evaluate (rotations $ (reverse . digits) n)

rotationFilter :: Int -> Bool
rotationFilter p = all isPrime ds
  where ds = getRotations p

result = filter rotationFilter (primesTo 1000000)
