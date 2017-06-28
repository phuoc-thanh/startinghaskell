{-# OPTIONS_GHC -Wall #-}
module Problem003 where

--The prime factors of 13195 are 5, 7, 13 and 29.

--What is the largest prime factor of the number 600851475143 ?

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
    | factors == []  = [n]
    | otherwise = factors ++ primeFactors (div n (head factors))
        where factors = take 1 $ filter (\x -> (mod n x) == 0) [2 .. n-1]

--https://en.wikipedia.org/wiki/Prime_factor
--https://www.mathsisfun.com/prime-factorization.html