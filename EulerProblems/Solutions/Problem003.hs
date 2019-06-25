{-# OPTIONS_GHC -Wall #-}
module Problem003 where

--The prime factors of 13195 are 5, 7, 13 and 29.

--What is the largest prime factor of the number 600851475143 ?

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
    | fst_fact n 2 == n  = [n]
    | otherwise = (fst_fact n 2) : primeFactors (div n (fst_fact n 2))
        where fst_fact x y = if (mod x y == 0) then y else fst_fact x (y+1)

--https://en.wikipedia.org/wiki/Prime_factor
--https://www.mathsisfun.com/prime-factorization.html