{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap (fib) [0..30]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs
-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons val stream) = f val `Cons` (fmap f stream)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys  = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake i = take i . streamToList

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+ 1) 0

ruler :: Stream Integer
ruler = rulerCore 0 where
        rulerCore x = sInterleave (sRepeat x) (rulerCore (x + 1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand

rand :: Int -> Stream Int
rand s = Cons c $ rand c
    where c = (1103515245 * s + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 223 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = go Nothing where
    go p []      = p
    go !p (x:xs) = go (comp p x) xs

comp :: Ord t => Maybe (t, t) -> t -> Maybe (t, t)    
comp p x = case p of
    Nothing -> Just (x, x)
    Just (mi, ma)
        | x < mi -> Just (x, ma)
        | x > ma -> Just (mi, x)
        | otherwise -> Just (mi, ma)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined

--thanks to https://gist.github.com/scott-fleischman/b5b401cd386b9e75305f