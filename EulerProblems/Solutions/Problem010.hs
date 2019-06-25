--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

--Find the sum of all the primes below two million.

primes10 = [2,3,5,7]

-- why 120?, coz 11^2 = 121
primes120 :: [Integer]
primes120  = primes10 ++ sieveofEratosthenes primes10 [11..120]

-- why 1415?, coz 1415^2 close to 2000000
primes1415 :: [Integer]
primes1415 = primes120 ++ sieveofEratosthenes primes120 [121..1415]

-- call sum primes2000000, it get the result within 5 seconds
primes2000000 :: [Integer]
primes2000000 = primes1415 ++ sieveofEratosthenes primes1415 [1416..2000000]

sieveofEratosthenes :: [Integer] -> [Integer] -> [Integer]
sieveofEratosthenes (x:xs) ys = sieveofEratosthenes xs (filter (\m -> mod m x /= 0) ys)
sieveofEratosthenes [] ys = ys