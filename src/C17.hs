module C17
  (result)
  where

writePrefix :: Int -> [Char]
writePrefix 0 = ""
writePrefix 2 = "twen"
writePrefix 3 = "thir"
writePrefix 4 = "for"
writePrefix 5 = "fif"
writePrefix 8 = "eigh"
writePrefix n = writeUnit n


writeUnit :: Int -> [Char]
writeUnit 0 = "zero"
writeUnit 1 = "one"
writeUnit 2 = "two"
writeUnit 3 = "three"
writeUnit 4 = "four"
writeUnit 5 = "five"
writeUnit 6 = "six"
writeUnit 7 = "seven"
writeUnit 8 = "eight"
writeUnit 9 = "nine"
writeUnit _ = ""

writeDec :: Int -> [Char]
writeDec 10 = "ten"
writeDec 11 = "eleven"
writeDec 12 = "twelve"
writeDec 14 = "fourteen"
writeDec n = case n < 10 of
  True  -> writeUnit n
  False -> case n < 20 of
    True -> writePrefix (n - 10) ++ "teen"
    False -> (writePrefix dec) ++ "ty" ++ (if unit == 0 then "" else "-" ++ writeUnit unit)
      where dec = n `div` 10
            unit = n `mod` 10

writeHun :: Int -> [Char]
writeHun n = writeUnit hun ++ " hundred" ++ (if dec == 0 then "" else " and " ++ writeDec dec)
  where
    hun = n `div` 100
    dec = n `mod` 100

writeNum :: Int -> [Char]
writeNum n = case n > 9 of
  False -> writeUnit n
  True  -> case n > 99 of
    False -> writeDec n
    True  -> writeHun n

big = (foldr1 (\x -> \y -> x ++ " " ++ y) (map writeNum [1..999])) ++ " one thousand"
result = length $ filter (\x -> x /= ' ' && x /= '-') big
