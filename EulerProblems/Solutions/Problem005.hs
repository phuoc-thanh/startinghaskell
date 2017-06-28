--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


-- call smallestMultiple [20,40..400000000] and wait to the result
smallestMultiple :: [Integer] -> [Integer]
smallestMultiple [] = []
smallestMultiple (x:xs)
    | div1120 x == 0 = x:smallestMultiple xs
    | otherwise = smallestMultiple xs

div1120 :: Integer -> Integer
div1120 n = sum $ map (mod n) [11,12..19]