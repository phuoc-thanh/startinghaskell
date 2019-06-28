module Searching where

-- Linear Search
-- O(n)
li_search x [] = Nothing
li_search x (y:ys) = if (x == y) then Just x else li_search x ys 


-- Binary Search, for sorted list only
-- O(logn)
bi_search x [y] = if (x == y) then Just x else Nothing
bi_search x ys
    | x < mid = bi_search x left
    | x > mid = bi_search x right
    | otherwise = Just x
    where (left, right) = splitAt (div (length ys) 2) ys
          mid           = head right
