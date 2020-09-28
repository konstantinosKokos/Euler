module C28
  (result)
  where

takeEvery :: Int -> Int -> [Int] -> [Int]
takeEvery _ _ [] = []
takeEvery d 0 (x1:x2:xs) = takeEvery (d+2) 4 xs
takeEvery d m (x:xs) = x:takeEvery d (m-1) (drop (d-1) xs)


result = sum (takeEvery 2 4 [3..1001^2]) + 1
