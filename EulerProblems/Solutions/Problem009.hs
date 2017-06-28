-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

import Data.Functor

--put more seeds if can't find the result
seedTriples :: [(Int,Int,Int)]
seedTriples = [(3, 4, 5), (5, 12, 13), (8, 15, 17), (7, 24, 25)]

pytagore :: [Int] -> (Int, Int, Int) -> [(Int,Int,Int,Int)]
pytagore (x:xs) (a,b,c) =  (x*a, x*b, x*c, x*a + x*b + x*c) : (pytagore xs (a,b,c))
pytagore [] _ = []

theOnlyOne :: [(Int,Int,Int)] -> [(Int,Int,Int,Int)]
theOnlyOne ((a,b,c):(ys)) = filter (\(x,y,z,w) -> w == 1000) $ (pytagore [1..99] (a,b,c)) ++ theOnlyOne ys
theOnlyOne [] = []

--call theOnlyOne seedTriples

-- Without programming:

-- a= 2mn; b= m^2 -n^2; c= m^2 + n^2;
-- a + b + c = 1000;

-- 2mn + (m^2 -n^2) + (m^2 + n^2) = 1000;
-- 2mn + 2m^2 = 1000;
-- 2m(m+n) = 1000;
-- m(m+n) = 500;

-- m>n;

-- m= 20; n= 5;

-- a= 200; b= 375; c= 425;