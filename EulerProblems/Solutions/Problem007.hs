--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?

isPrime :: Integer -> [Integer] ->Bool
isPrime n xs = (length . filter (==0) $ map (mod n) xs) == 1

seedPrimes :: [Integer]
seedPrimes = [3,5,7]

--call [2,3,5,7] ++ nPrime [11,13..104749] (seedPrimes)
--around 45s to get the result
nPrime :: [Integer] -> [Integer] -> [Integer]
nPrime [] _ = []
nPrime (n:ns) xs
    | isPrime n (n:xs) = n : nPrime ns (n:xs) 
    | otherwise = nPrime ns xs


-- go to problem 10 to get Primes Sieve version that have a faster implementation.