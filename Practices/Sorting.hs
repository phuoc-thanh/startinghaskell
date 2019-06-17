{-# LANGUAGE OverloadedStrings #-}

module Sorting where


-- In-efficient sorts will be skipped (selection, insertion...)
-- Only efficient sorts and not-imperative sorts are implemented:
-- Bubble Sort | Merge Sort | Quick Sort | Radix Sort

arr = [170, 45, 75, 90, 02, 802, 2, 66]

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
-- Time Complexity : O(n log n)
-- Do merge (n) times, each merge takes (log n) steps
-- Space Complexity: O(n), or O(1) with linked list *
-- Can optimize with Natural Merge, which divide elems to sorted sets
-- The the best case of it, should be O(n)
merge_sort (x:[]) = [x]
merge_sort xs  = merge (merge_sort (fst m))  (merge_sort (snd m)) where
    m          = splitAt (div (length xs) 2) xs
    merge x [] = x
    merge [] y = y
    merge x y
        | head x > head y = head x : (merge (tail x) y)
        | otherwise       = head y : (merge x (tail y))

-- | Quick Sort
-- Time Complexity : O(n^2) for the worse case, O(n log n) for best/average cases
-- Space Complexity: O(n) naive implementation
-- Can optimize with tail recursive and smarter partioning (i.e choose pivot at the middle of list)
quick_sort [] = []
quick_sort (x:xs) = (quick_sort left) ++ [x] ++ (quick_sort right) where
    left  = filter (>= x) xs
    right = filter (<  x) xs

-- Non Comparison-based
radix_sort (x:xs) = map (\x -> mod x 10) (x:xs) 

-- n times power of 10
radix n xs  = filter (\x -> div x (10^n) == 0) xs
