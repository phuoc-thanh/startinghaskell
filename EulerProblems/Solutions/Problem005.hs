--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- Do loop with 20 leap steps.
-- Call smallestMultiple [20,40..240000000] to get result
smallestMultiple :: [Integer] -> [Integer]
smallestMultiple [] = []
smallestMultiple (x:xs)
    | div1120 x == 0 = x:smallestMultiple xs
    | otherwise = smallestMultiple xs

-- Find if a number can divide by [1..19] without remaining
-- div1120 x == 0 -> x is qualified
div1120 :: Integer -> Integer
div1120 n = sum $ map (mod n) [11,12..19]