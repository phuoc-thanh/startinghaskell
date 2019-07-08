module Lists where

-- https://www.hackerrank.com/challenges/new-year-chaos
-- Bad performance if applying on large list

minimumBribes xs
    | res == 0  = "Too chaotic"
    | otherwise = show res
    where res = f 0 (zipWithIndex xs)

f n [] = n
f n (x:xs)
    | fst x - snd x > 2 = f 0 []
    | otherwise = f (n + overtakes) xs
    where overtakes = sum $ map (\a -> overtake x a) xs

overtake x y
    | fst x > fst y && snd x < snd y = 1
    | otherwise = 0

zipWithIndex xs = zip xs [1..(length xs)]