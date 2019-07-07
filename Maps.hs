module Maps where

import Prelude hiding (lookup)
import Data.Map.Strict

-- https://www.hackerrank.com/challenges/ctci-ransom-note/

checkMagazine magazin note
    | res == 0 = putStrLn "Yes"
    | otherwise = putStrLn "No"
    where res = f (fromListWith (+) $ zipCount magazin) note

f w [] = 0
f w (n:ns)
    | lookup n w == Nothing = 1
    | lookup n w == (Just 0) = 1
    | otherwise = f (adjust (+ (-1)) n w) ns

zipCount xs = zip xs (cycle [1])