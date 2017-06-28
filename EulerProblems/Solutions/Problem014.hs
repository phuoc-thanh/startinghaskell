
-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Data.List

collatzSeqLength :: Int -> Int
collatzSeqLength n = length $ collatzSequence n
    where collatzSequence :: Int -> [Int]
          collatzSequence 1 = [1]
          collatzSequence n
            | even n = n : collatzSequence (div n 2)
            | otherwise = n : collatzSequence (3*n + 1)

longestCollatzSequence :: Int -> Int
longestCollatzSequence n = maximum $ map (collatzSeqLength) [3,5..n]
-- get the index i then starting number would be (2*i + 3)
-- elemIndex 525 (map collatzSeqLength [3,5..999999])