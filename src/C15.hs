module C15
  (result)
  where

import Data.Array

m = 20
grid = [(x, y) | x <- [0..m], y <- [0..m]]

numPaths :: Array (Int, Int) Int
numPaths = listArray ((0, 0), (m,m)) $ map countPaths grid

countPaths :: (Int, Int) -> Int
countPaths (0, 0) = 0
countPaths (dx, 0) = if dx > 0 then 1 else 0
countPaths (0, dy) = if dy > 0 then 1 else 0
countPaths (dx, dy) = case dx < 0 || dy < 0 of
  True  -> 0
  False ->
    (if inRange (bounds numPaths) (dx-1, dy)
    then numPaths ! (dx-1, dy)
    else countPaths(dx-1, dy)) +
    (if inRange (bounds numPaths) (dx, dy-1)
    then numPaths ! (dx, dy-1)
    else countPaths(dx, dy-1))

result = numPaths ! (m, m)
