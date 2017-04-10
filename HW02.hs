{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches (_:_) [] = 0
exactMatches xs ys = length . filter (==True) $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = [length $ filter (==c) code | c <- colors]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith min (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y m (n - m)
    where m = exactMatches x y
          n = matches x y

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs m n) ys = m == m' && n == n'
    where Move _ m' n' = getMove xs ys

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m xs = filter (isConsistent m) xs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [c : code | c <- colors, code <- allCodes (n - 1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = choose c $ allCodes 4

choose :: Code -> [Code] -> [Move]
choose _ [] = []
choose c (x:xs)
    | x == c = [m]
    | otherwise = m : (choose c $ filterCodes m xs)
    where m = getMove c x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

-- Thanks to https://gist.github.com/adolfopa/2cb09306d4c287719601
