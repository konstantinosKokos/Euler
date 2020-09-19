module C6
    ( result
    ) where

result = 2 * (sum [x * (sum [x+1..100]) | x <- [1..100]])
