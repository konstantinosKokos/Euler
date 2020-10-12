module C39
    (
    result
    ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

gen :: Int -> [(Int, Int, Int)]
gen s = [(a, b, (s-a-b)) | a <- [1..s `div` 3], b <- [a..(s-a) `div` 2]]

isRightTriangle :: (Int, Int, Int) -> Bool
isRightTriangle (a, b, c) = a^2+b^2 == c^2

getNumSolutions :: Int -> Int
getNumSolutions n = length $ filter isRightTriangle (gen n)

result = maximumBy (comparing getNumSolutions) [2..1000]
