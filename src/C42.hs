module C42
    (
    result
    ) where

import            System.IO
import            C12 (triangles)
import            Data.Maybe
import            Data.List (elemIndex)
import            Data.Char (toLower)
import            Data.List.Split (splitOn)

triangles' = map snd triangles

alphabet = "abcdefghijklmnopqrstuvwxyz"

letterToPos :: Char -> Int
letterToPos x = case elemIndex (toLower x) alphabet of
  Just n  -> n + 1
  Nothing -> 0

wordToValue :: [Char] -> Int
wordToValue xs = sum (map letterToPos xs)

triangleNumber :: Int -> Bool
triangleNumber n = n == last (takeWhile (<=n) triangles')

result :: IO Int
result = do
  names     <- readFile "./data/p042_words.txt"
  return (length $ filter triangleNumber (map wordToValue (splitOn "," names)))
