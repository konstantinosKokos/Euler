module C9
    ( result
    ) where


pairs = [ (x, 1000 - x) | x <- [1..666]]

expandPair :: (Int, Int) -> [(Int, Int, Int)]
expandPair (sides, hypo) = [(x, sides - x, hypo) | x <- [1..(sides `div` 2)]]
expanded_pairs = foldr (++) [] $ map expandPair pairs

isHypotenuse :: (Int, Int, Int) -> Bool
isHypotenuse (x, y, z) = x*x + y*y == z*z

mult_three :: (Int, Int, Int) -> Int
mult_three (x, y, z) = x * y * z

result  = mult_three $ head $ filter isHypotenuse expanded_pairs
