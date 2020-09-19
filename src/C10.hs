module C10
    ( result
    ) where

import C3 (primes)

result = sum $ takeWhile (\x -> x < 2000000) primes
