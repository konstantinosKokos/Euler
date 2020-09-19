module C23
  (result)
  where

import C21 (sumProperDivisors)
import Control.Monad.State
import Data.List.Ordered

type AbundantState = ([Int], Int)

abundant :: Int -> Bool
abundant n = sumProperDivisors n > n

abundants = map abundant [1..28123]

isNotPairSum :: Int -> [Int] -> Bool
isNotPairSum n xs = all (\x -> not $ member (n-x) xs) xs

getSumNonAbundants :: [Int] -> State AbundantState Int
getSumNonAbundants [] = do
  (_, total) <- get
  return total
getSumNonAbundants (n:ns) = do
  (abundants, total) <- get
  put (if abundant n then abundants ++ [n] else abundants,
      total + if isNotPairSum n (takeWhile (<n) abundants) then n else 0)
  getSumNonAbundants ns

start_state :: AbundantState
start_state = ([], 0)

result = evalState (getSumNonAbundants [1..28123]) start_state
