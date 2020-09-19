module C12
    ( result, triangles, numOccs
    ) where

import C3 (primeFactors)
import Data.List (nub)

type Triangle = (Int, Int)

nextTriangle :: Triangle -> Triangle
nextTriangle (idx, s) = (idx+1, s + idx + 1)

triangles = iterate nextTriangle (1,1)


numOccs :: Eq a => [a] -> a -> Int
numOccs xs x = (length . filter (==x)) xs

countDivisors :: Int -> Int
countDivisors n =
  foldr1 (*) (map (+1) exponents)
  where exponents = map (numOccs fs) (nub fs)
        fs = primeFactors n


result = head $ filter (\x -> countDivisors x >= 500) $ map snd triangles
