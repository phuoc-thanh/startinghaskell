{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P f1) (P f2) = (dropWhileEnd (==0) f1) == (dropWhileEnd (==0) f2)

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P f) = showP $ dropWhileEnd (==0) (reverse f)

showP :: (Num a, Eq a, Show a) => [a] -> String
showP [] = ""
showP (z:zs)
    | zs == [] = show z
    | z == 0 = showP zs
    | z == 1 = "x" ++ "^" ++ show (length zs) ++ " + " ++ showP zs
    | length zs == 1 = show z ++ "x" ++ " + " ++ showP zs
    | otherwise = show z ++ "x" ++ "^" ++ show (length zs) ++ " + " ++ showP zs

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

