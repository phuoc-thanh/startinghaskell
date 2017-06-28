-- https://projecteuler.net/problem=15

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

-- Compute Binomial Coefficient
choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = div (choose (n-1) (k-1) * n) k

-- consider a (m x n) grid (ie 2x2 or 20x20)
-- we must move (m + n) steps from the start point to end point
-- consist of m steps of move down and n steps of move right
-- we have to pick m (or n) from (m + n) DIFFERENTLY (or unordered) 
-- this why we go for Binomial Coefficient

-- lattice path of grid 20
lattice20x20 :: Integer
lattice20x20 = choose 40 20


--https://en.wikipedia.org/wiki/Binomial_coefficient
--https://en.wikipedia.org/wiki/Lattice_path
--http://mathworld.wolfram.com/BinomialCoefficient.html