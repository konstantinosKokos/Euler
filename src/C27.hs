module C27
  (result, isPrime)
  where

import Data.Ord (comparing)
import Data.List (maximumBy)


isPrime :: Int -> Bool
isPrime 0 = False
isPrime n = not $ any (\x -> n `rem` x == 0) [2..floor $ sqrt $ fromIntegral n]



countLen :: Int -> Int -> Int
countLen a b =  length $ takeWhile (\x -> isPrime $ x^2 + a*x + b) [0..]

cs :: [(Int, Int)]
cs = [(a, b) | a <- [-999..999], b <- filter isPrime [0..999]]

result =  snd $ maximumBy (comparing fst)
                         $ map (\p -> (uncurry countLen p, p)) cs
