-- Polymorphism and Functional Programming Paradigms
-- CIS 194 Spring 2015, Lecture 02
-- http://www.cis.upenn.edu/~cis194/spring15/lectures/02-lists.html


-- let expression
letPlus :: Int -> Int -> Int
letPlus x y = let r = 3*x
                  s = 6*y
                  in r + s

strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let l = strLength xs
                       in l + 1

-- where clause
frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
    | len > 5 = 'x'
    | len < 3 = 'y'
    | otherwise = 'z'
    where len = strLength str

-- Accumulator
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums   -- the acc. starts at 0
    where go :: Int -> [Int] -> Int
          go c [] = c
          go c (x:xs)
            | c >= 20 = c
            | otherwise = go (c + x) xs

-- Recursion Patterns: Map, Filter, Fold
exampleList = [-1, 2, 6]
a = map (+1) exampleList
b = map (*2) exampleList
c = filter even exampleList
d = filter (>0) exampleList
-- fold left (from left) with initvalue = 1. Operation (+) is an associative operation
-- ref: https://en.wikipedia.org/wiki/Monoid
e = foldl (*) 1 exampleList

-- Functional Combinator
negateNumEvens2 :: [Int] -> Int
negateNumEvens2 xs = negate $ length $ filter even xs

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (>3)

-- Lambda
duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)