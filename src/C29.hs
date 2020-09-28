module C29
  (result)
  where

import Data.Set (fromList, size)

result = size $ fromList [x^y | x <- [2..100], y <- [2..100]]
