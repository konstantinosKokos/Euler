module C19
  (result)
  where

type Date = (Int, Int, Int, Int)

isSundayFirst :: Date -> Bool
isSundayFirst (1, _, _, 7) = True
isSundayFirst (_, _, _, _) = False

addWeekday :: Int -> Int
addWeekday 7 = 1
addWeekday n = n+1

isLeap :: Int -> Bool
isLeap year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4   == 0 = True
  | otherwise           = False

getNumDays :: (Int, Int) -> Int
getNumDays (2, year) = case isLeap year of
  True  -> 29
  False -> 28
getNumDays (month, _) = case elem month [1,3,5,7,8,10,12] of
  True  -> 31
  False -> 30

addDate :: Date -> Date
addDate (day, month, year, weekday) = case day < num_days of
  True  ->  (day + 1, month, year, next_day)
  False -> case month < 12 of
    True -> (1, month + 1, year, next_day)
    False -> (1, 1, year + 1, next_day)
  where num_days = getNumDays (month, year)
        next_day = addWeekday weekday


all_dates = iterate addDate (1, 1, 1900, 1)

result = length $
  filter
  (\(day, _, year, weekday) ->
    day == 1 && year > 1900 && weekday == 7)
  $
  takeWhile (\(_, _, year, _) -> year < 2001) all_dates
