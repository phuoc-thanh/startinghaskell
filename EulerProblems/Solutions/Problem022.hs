-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
--  begin by sorting it into alphabetical order. Then working out the alphabetical value for each name,
--  multiply this value by its alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN,
--  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
--  So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?
module Problem022 (main) where

import Data.Char
import Data.List.Split
import Data.List

main :: IO Int
main = euler_p22

euler_p22 :: IO Int
euler_p22 = do
    names <- readFile "p022_names.txt"
    return $ sumNames $ sort $ map posChar $ splitOn "," $ filter (\x -> not (x `elem` "\"")) names

posChar :: String -> [Int]
posChar xs = map ((+ (-64)) . ord) xs

sumNames :: [[Int]] -> Int
sumNames [] = 0
sumNames (x:xs) = sum [p * q | (p, q) <- zipWithIndex $ map sum (x:xs)]

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [1,2..]
