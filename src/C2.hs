{-# LANGUAGE BangPatterns #-}

module C2
    ( result
    ) where

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
result = sum (takeWhile (\ x -> x < 4000000)
                       $ filter (\ x -> x `mod` 2 == 0 ) fibs)

