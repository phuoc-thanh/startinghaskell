-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
-- The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

multiples35 :: Int -> Bool
multiples35 n = (n `mod` 3 == 0) || (n `mod` 5 == 0)

sumList :: Int
sumList = sum $ filter (multiples35) [1..999]