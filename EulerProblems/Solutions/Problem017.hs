-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
--  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 
-- (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

letters :: Integer -> String
letters 1 = "One"
letters 2 = "Two"
letters 3 = "Three"
letters 4 = "Four"
letters 5 = "Five"
letters 6 = "Six"
letters 7 = "Seven"
letters 8 = "Eight"
letters 9 = "Nine"
letters 10 = "Ten"
letters 11 = "Eleven"
letters 12 = "Twelve"
letters 13 = "Thirteen"
letters 14 = "Fourteen"
letters 15 = "Fifteen"
letters 18 = "Eighteen"
letters 20 = "Twenty"
letters 30 = "Thirty"
letters 40 = "Forty"
letters 50 = "Fifty"
letters 60 = "Sixty"
letters 70 = "Seventy"
letters 80 = "Eighty"
letters 90 = "Ninety"
letters 100 = "OneHundred"
letters 1000 = "OneThousand"
letters n
    | (n > 100 && mod n 100 == 0) = letters (div n 100) ++ "Hundred"
    | (n > 100) = letters (div n 100) ++ "HundredAnd" ++ letters (mod n 100)
    | (n > 20) = letters (div n 10 * 10) ++ letters (mod n 10)
    | (n > 15) = letters (mod n 10) ++ "teen"
    | otherwise = ""

lettersCount :: [Integer] -> Int
lettersCount xs = length $ concat $ map letters xs