{-# LANGUAGE FlexibleContexts #-}

module Sorting where

import Data.Bits 

-- In-efficient sorts will be skipped (selection, insertion...)
-- Only efficient sorts and not-imperative sorts are implemented:
-- Bubble Sort | Merge Sort | Quick Sort | Radix Sort

arr = [170, 45, 75, 90, 2, 802, 2, 66]

-- Maximum of a list, can be done with one bubble (swap method) sort
-- Or with default Prelude func
-- max_ xs = maximum
max_ (x:[])     = x
max_ (x:y:xs)
    | x > y     = max_ (x:xs)
    | otherwise = max_ (y:xs)

-- Comparison-Based Sort Algorithms, include Bubble Sort, Merge Sort, Quick Sort

-- | Bubble Sort
-- Time Complexity : O(n^2) in theory, but this implementation is: n + (n-1) + .. + 1.
-- Time Complexity : T = n*(n+1) / 2
-- Space Complexity: Hard to say due to immutability of Haskell and GHC behavior
-- Can optimize with check flag if bubble xs == xs then break
-- The best case of optimized algorithm is O(n) with sorted array
-- The worst case is still O(n^2)
bubble_sort [] = []
bubble_sort xs = last (bubble xs) : bubble_sort (init $ bubble xs) where
    bubble (x:[])     = [x] 
    bubble (x1:x2:xs) = min x1 x2 : bubble ((max x1 x2) : xs)

-- | Merge Sort
-- Time Complexity : O(n log n) in theory, but using merge list may be more efficient in some cases
-- Analyse1: Do merge (n) times, each merge requires (log n) dividing steps
-- Analyse2: Do (log n) merge steps, each step takes n (linear) comparison merges
-- Space Complexity: O(n), or O(1) with linked list *
-- Can optimize with Natural Merge, which divide elems to sorted sets
-- The the best case of it, should be O(n)
merge_sort (x:[]) = [x]
merge_sort xs     = merge (merge_sort left) (merge_sort right) where
    (left, right) = splitAt (div (length xs) 2) xs


-- Merge 2 sorted lists/arrays function, by descending
-- O(n)
merge x [] = x
merge [] y = y
merge x y
    | head x > head y = head x : (merge (tail x) y)
    | otherwise       = head y : (merge x (tail y))

-- | Quick Sort
-- Time Complexity : O(n^2) for the worse case: already sorted, pivot num always the greatest or lowest.
-- Time Complexity : O(n log n) for best case: pivot number is in the mid, left ~ right
-- Space Complexity: O(n) naive implementation
-- Can optimize with tail recursive and smarter partioning (i.e choose pivot at the middle of list)
quick_sort [] = []
quick_sort (x:xs) = (quick_sort left) ++ [x] ++ (quick_sort right) where
    left  = filter (>= x) xs
    right = filter (<  x) xs

-- Non Comparison-based
-- Most Significant Digit - MSD Radix Sort
-- Time Complexity O(w.n) with w = length of word/key
-- Time n Space Complexity of Radix Sort is still open discussion
radix_sort [] = []
radix_sort xs = msd_sort (msd mx) l ++ radix_sort r where
    (l, r)    = partition (\x -> base x == base mx) xs
    mx        = max_ xs

-- Most Significant Digit Sort, for list of same base numbers
msd_sort 0 xs = xs
msd_sort n xs = l ++ msd_sort (n - 1) r where
    (l, r)    = partition (\x -> msd x == n) xs

-- Partition by predicate p 
partition p xs = (filter p xs, filter (not . p) xs)
-- Most Significant Digit of a number
msd n  = if n < 10 then n else msd (div n 10)
-- Calculate radix (base) of a number
base n = if n < 10 then 1 else 10 * base (div n 10)

