{-# LANGUAGE OverloadedStrings #-}

module Sorting where


-- | Bubble Sort
-- Time Complexity = O(n^2) in theory, but this implementation is: n + (n-1) + .. + 1.
-- Time Complexity: T = n*(n+1) / 2
-- Space Complexity: Hard to say due to immutability of Haskell and GHC behavior
-- Can optimize with check flag if bubble xs == xs then break
-- The best case of optimized algorithm is O(n) with sorted array
-- The worst case is still O(n^2)
bubble_sort [] = []
bubble_sort xs = last (bubble xs) : bubble_sort (init $ bubble xs) where
    bubble (x:[])     = [x] 
    bubble (x1:x2:xs) = min x1 x2 : bubble ((max x1 x2) : xs)

-- | Merge Sort
-- incomplete
merge_sort (x:[]) = [x]
merge_sort xs  = merge (merge_sort (fst m))  (merge_sort (snd m)) where
    m          = splitAt (div (length xs) 2) xs
    merge x [] = x
    merge [] y = y
    merge x y
        | head x < head y = head x : (merge (tail x) y)
        | otherwise       = head y : (merge y (tail y))
