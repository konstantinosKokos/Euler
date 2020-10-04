module C38
    (cProd, result
    ) where

import C32 (isPandigit)
import Data.Char (digitToInt)
import Data.List (sort)

cProd :: Int -> Int -> Int -> [Int]
cProd f left num
  | left - (length ds) <= 0 = ds
  | otherwise               = ds ++ (cProd (f+1) (left - (length ds)) (num))
  where
    ds = map digitToInt $ show (f*num)

result = maximum $
  map (foldl (\x y -> 10 * x + y) 0) $
  filter (isPandigit [1..9]) (map (cProd 1 9) [1..9999])
-- mult n i vs
--     | length (concat vs) >= 9 = concat vs
--     | otherwise               = mult n (i+1) (vs ++ [show (n * i)])
--
-- problem_38 :: Int
-- problem_38 = maximum . map read . filter ((['1'..'9'] ==) . sort)
--                $ [mult n 1 [] | n <- [2..9999]]
