module C22
  (result)
  where

import Data.List
import Data.Map (fromList, (!), Map)

split :: (a -> Bool) -> [a] -> ([a], [a])
split f xs = (ls, rs)
  where
    (ls, rs') = break f xs
    rs
      | null rs'  = []
      | otherwise = tail rs'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn s xs = y:splitOn s ys
  where (y, ys) = split (==s) xs

scoreChar :: Map Char Int
scoreChar = fromList $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]

scoreName :: [Char] -> Int
scoreName xs = foldr (+) 0 (map (scoreChar!) xs)

keepBody n = dropWhile (=='\"') $ dropWhileEnd (=='\"') n

big_str = readFile "./data/p022_names.txt"
names = fmap sort $ (fmap . fmap) keepBody (fmap (splitOn ',') big_str)

name_scores :: IO [Int]
name_scores = (fmap . fmap) scoreName names

result = fmap (foldr1 (+)) $ fmap (zipWith (*) [1..]) name_scores
