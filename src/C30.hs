module C30
  (result)
  where

import Data.Char (digitToInt)

powerOfDigits :: Int -> Int -> Int
powerOfDigits n xs = sum (map (\x -> (digitToInt x)^n) (show xs))

result = sum (filter
              (\x -> 5 `powerOfDigits` x == x)
              [2..(9^5*4)])
