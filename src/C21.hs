module C21
  (result, sumProperDivisors)
  where

import C3 (primeFactors)
import C12 (numOccs)
import Data.List (nub)
import Control.Monad.State

primeContribution :: Int -> Int -> Int
primeContribution 1 _ = 1
primeContribution p n = (p ^ (n+1) - 1) `div` (p - 1)

sumProperDivisors :: Int -> Int
sumProperDivisors 0 = 0
sumProperDivisors 1 = 0
sumProperDivisors n = (foldr1 (*)
  (map (\x -> primeContribution x (numOccs fs x)) (nub fs))) - n
  where fs = primeFactors n

type AmicableState = ([Int], Int)
start_state :: AmicableState
start_state = ([], 0)

getSumDivisors :: [Int] -> State AmicableState Int
getSumDivisors [] = do
  (_, total) <- get
  return total
getSumDivisors (n:ns) = do
  (ignored, total) <- get
  unless (elem n ignored) (do
      let dn = sumProperDivisors n
      let ddn = sumProperDivisors dn
      when (n==ddn && n /=dn) (put ([dn] ++ ignored, n + dn + total))
    )
  getSumDivisors ns

result = evalState (getSumDivisors [1..10000]) start_state
