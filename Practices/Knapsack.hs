-- The issue was found in a code challenge, part of Zalora recruitment process.
-- Wiki link: https://en.wikipedia.org/wiki/Knapsack_problem
-- NP Completeness: https://en.wikipedia.org/wiki/NP-completeness
-- There are several types of Knapsack: 0-1 knapsack, un-bounded knapsack, bounded knapsack...

-- The decision problem form of Knapsack is N-P complete (nondeterministic polynomial time),
-- thus there is no known algorithm both correct and fast (polynomial-time) in all cases.
-- The optimization problem is NP-hard, its resolution is at least as difficult as the decision problem,
-- and there is no known polynomial algorithm which can tell, given a solution.

-- There is a pseudo-polynomial time algorithm using dynamic programming.
-- That's the main focus of this implementation.

module Knapsack where

-- Sample sets of 0/1 knapsack
wlim    = 10
values  = [1, 3, 2, 2, 3, 4]
weights = [5, 7, 6, 1, 2, 3]
-- 0 1 2 3 4 5 6 7 8 9 10
-- 0 0 0 0 0 0 0 0 0 0 0
-- 0 0 0 0 0 1 1 1 1 1 1
-- 0 0 0 0 0 1 1 3 3 3 3
-- 0 0 0 0 0 1 2 3 3 3 3
-- 0 2 2 2 2 2 3 4 5 5 5
-- 0 2 3 5 5 5 5 5 6 7 8
-- 0 2 3 4 6 7 9 9 9 9 9  

-- Sample sets 2 of 0/1 knapsack
wlim2    = 67
values2  = [505, 352, 458, 220, 354, 414, 498, 545, 473, 543]
weights2 = [23,  26,  20,  18,  32,  27,  29,  26,  30,  27 ]

-- The Solution can be found on Wiki or other links.
-- But I try to give a simplest explanation.

-- When you pick an item /i/ of items, assume that 'W' is original weight limit, 'w' is the limit of /i/
-- there are exactly 2 potentials:

-- 1. You've chosen the right item (a.k.a /i/ is belong the result set),
-- then you should continue choose the next items with the weight limit of (W - w)
-- 2. You've chosen the wrong item (a.k.a /i/ is not belong to result set),
-- then you should exclude /i/, work with remaining items and the original weight limit

-- So the solution is: max (1) (2)
-- Time complexity: Tn = 2T(n-1) = 4T(n-2) = 8T(n-3) = .. = O(2^n)
-- choose :: limit -> weights -> values -> Result
choose 0 _ _  = 0 -- zero limit case
choose _ _ [] = 0 -- empty values case
choose _ [] _ = 0 -- empty weights case
choose l (w:ws) (v:vs)
    | w > l      = choose l ws vs
    | otherwise  = max (v + choose (l - w) ws vs) (choose l ws vs)


-- Using Dynamic Programming, save the result to a table

