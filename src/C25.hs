module C25
  (result)
  where

import C2 (fibs)

idxes :: [Int]
idxes =  [1..]

indexed_fibs = zip idxes fibs

digitLength :: (Num a, Show a) => a -> Int
digitLength n = length (show n)

result = (+) 1 $ fst . head $ filter (\x -> digitLength (snd x) == 1000) indexed_fibs
