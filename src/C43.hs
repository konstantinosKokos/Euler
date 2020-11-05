module C43
    (
    result
    ) where

import          C3              (primes)
import          C32             (pandigits, evaluate)
import          Data.List
import          Data.Maybe
import          Data.Char       (digitToInt)

pandigits' = pandigits "0123456789"

substrings :: Int -> [Int]
substrings n = map evaluate (substrings' 3 (show' sn))
  where sn        = show n
        show' xs  = case length xs of
          10 -> drop 1 xs
          9  -> xs

recSplitAt :: Int -> [a] -> Maybe ([a], [a])
recSplitAt _ []     = Nothing
recSplitAt n (x:xs) = Just (pre, rest)
  where pre = take n (x:xs)
        rest = case length xs < n of
          True  -> []
          False -> xs

substrings' :: Int -> [a] -> [[a]]
substrings' n c = unfoldr (recSplitAt n) c

primeDivisible :: [Int] -> Bool
primeDivisible nums = all
  (\(num, prime) -> mod num prime == 0)
  (zip nums primes)

result = sum (filter (primeDivisible.substrings) pandigits')
