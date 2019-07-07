module Sets where

import Data.Set

-- https://www.hackerrank.com/challenges/sock-merchant

ar = [1,2,1,2,1,3,2] -- 2 pairs: 1,1 and 2,2
setR = empty

f [] s = 0
f (x:xs) s
    | member x s = 1 + f xs (delete x s)
    | otherwise  = f xs (insert x s)

