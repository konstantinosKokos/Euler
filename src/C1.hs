module C1
    ( result
    ) where

result :: Int
result = sum $ filter (\ x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]

