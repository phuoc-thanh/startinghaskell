-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.Function (on)
import Data.List (sortBy)

palindrome = sortBy (compare `on` fst) [ (x*y, x) | x <- [100..999], y <- [100..999], reverse (show (x*y)) == show (x*y)]

-- thanks to http://stackoverflow.com/questions/1258145/palindromes-in-haskell
