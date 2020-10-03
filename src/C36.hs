module C36
  (result)
  where

import C34 (get_digs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ds = ds == reverse ds


isDoublePalindrome :: Int -> Bool
isDoublePalindrome d = isPalindrome ds && isPalindrome bs
  where ds = get_digs d
        bs = binarize d


data Binary = O | I
  deriving Eq

instance Show Binary where
  show O = "0"
  show I = "1"

binarize :: Int -> [Binary]
binarize 0 = []
binarize n = binarize q ++ [if r == 1 then I else O]
  where (q, r) = quotRem n 2

result = sum $ filter isDoublePalindrome [1..1000000]
