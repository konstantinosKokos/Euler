module C32
  (result, isPandigit, pandigits)
  where

import Data.Char (digitToInt, intToDigit)
import Data.List (permutations, delete, nub, sort)
import Control.Monad (liftM2, liftM)


digits = map intToDigit [1..9]

evaluate ::  [Char] -> Int
evaluate ds = foldl1 (\x y -> 10 * x + y) (map digitToInt ds)

attach_snd :: a -> ([a], [a]) -> ([a], [a])
attach_snd x (ls, rs) = (ls, x:rs)

revert (a, b) = (b, a)

split ::  Eq a => [a] -> [([a], [a])]
split []       = []
split (d:ds)   = nub $ ps ++ (map revert ps)
  where
    ps = [splitAt i (d:ds) | i <- [1..length ds]] ++
          (map (attach_snd d) (split ds))


trisplit :: Eq a => ([a], [a]) -> [(([a], [a]), [a])]
trisplit (xs, ys) =[((xs, zs), ws) | (zs, ws) <- split ys]


all_splits =
  filter
  (\s ->
    length ((fst.fst) s) == 1 && length ((snd.fst) s) == 4
    ||
    length ((fst.fst) s) == 2 && length ((snd.fst) s) == 3
  )
  (concat $ map trisplit (split digits))


pandigits :: [Char] -> [Int]
pandigits [] = []
pandigits xs = map evaluate (permutations xs)


all_combos =
  filter
  (\x ->
    (fst.fst) x < (snd.fst) x &&
    isPandigit (snd x) (show $ ((fst.fst) x) * ((snd.fst) x)))
  $
  concat $
  map
    (\x ->
      liftM (\y -> (y, snd x))
      (liftM2 (,) (pandigits $ (fst.fst) x) (pandigits $ (snd.fst) x)))
  all_splits

isPandigit :: Ord a => [a] -> [a] -> Bool
isPandigit fs xs = fs == sort xs

result = sum $ nub $map (\p -> f p) all_combos
  where
    f ((x, y), _) = x*y
