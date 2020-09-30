module C33
  (result)
  where

import C3 (primeFactors)
import Data.Maybe
import Data.List

double_digit :: [(Int, Int)]
double_digit = [(y, x) | x <- [11..99], y <- [11..(x-1)]]

curious :: (Int, Int) -> Bool
curious (x,y) = case simplify (x, y) of
  Just (z, w) -> div' (z, w) == div' (x,y)
  Nothing     -> False

div' :: (Int, Int) -> Double
div' (x,y) = (fromIntegral x)/(fromIntegral y)

simplify :: (Int,Int) -> Maybe (Int, Int)
simplify (x,y)
  | x2 == 0   = Nothing
  | x1 == y1  = Just (x2, y2)
  | x2 == y1  = Just (x1, y2)
  | x1 == y2  = Just (x2, y1)
  | x2 == y2  = Just (x1, y1)
  | otherwise = Nothing
  where
    (x1, x2) = quotRem x 10
    (y1, y2) = quotRem y 10

filtered = filter curious double_digit
(nom, den) = foldr1 (\p1 p2 -> ((fst p1 * fst p2), (snd p1 * snd p2))) filtered


result = (foldr (*) 1 noms, foldr1 (*) dens) where
  noms = noms' \\ dens'
  dens = dens' \\ noms'
  noms' = primeFactors nom
  dens' = primeFactors den
