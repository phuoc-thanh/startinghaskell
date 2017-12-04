{-# LANGUAGE BinaryLiterals #-}

import Data.List
import Data.Bits

decToBin 0 = [0]
decToBin 1 = [1]
decToBin n = decToBin (div n 2) ++ [(mod n 2)]

ex01 n = length $ filter (==1) $ decToBin n

ex02 n = (snd bin ++ fst bin)
    where bin = splitAt 4 $ decToBin n

bin9 = 0b1001

ifMatch a b c
    | (a == b) && (b == c) && (c == 1) = True
    | otherwise = False

ex03 n = decToBin n