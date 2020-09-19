module C4
    ( result
    ) where

import qualified Data.List.NonEmpty as NE

prods = NE.fromList [x*y | x <- [100..999], y <-[x..999]]
palis = NE.filter (\x -> reverse x == x) $ NE.map show $ NE.reverse $ NE.sort prods
result = head palis
