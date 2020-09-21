module C24
  (result)
  where

import Data.List (splitAt)

countPerms :: [a] -> Int
countPerms xs = foldr1 (*) [1..length xs]

moveToPerm' :: ([a], [a]) -> Int -> [a]
moveToPerm' ((xs), (y:ys)) n = y:moveToPerm (xs++ys) n

moveToPerm :: [a] -> Int -> [a]
moveToPerm xs 0 = xs
moveToPerm [] _ = []
moveToPerm (x:xs) n
  | n > seq_perms     = moveToPerm (x:xs) (seq_perms - n)
  | tail_perms > n    = x: moveToPerm xs n
  | otherwise         = moveToPerm' (splitAt perm_digit (x:xs)) new_n
  where seq_perms     = countPerms (x:xs)
        tail_perms    = countPerms xs
        perm_digit    = n `div` tail_perms
        new_n         = n `mod` tail_perms

digs :: [Int]
digs = [0..9]
result = foldl1 (\x -> \y -> 10 *x + y) $ (moveToPerm digs 999999)
