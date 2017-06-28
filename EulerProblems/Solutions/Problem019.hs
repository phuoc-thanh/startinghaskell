-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September, April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

import Data.List

isLeap 1900 = False
isLeap n = mod n 4 == 0

firstOfMonthNormalYear = [1,32,60,91,121,152,182,213,244,274,305,335]
firstOfMonthLeapYear = [1,32,61,92,122,153,183,214,245,275,306,336]

-- if 1st of Jan is Monday, assume that 1 is monday, and 7 is sunday
-- n is a day of week, n from 1 to 7.
-- be careful with the year that start with sunday, I just skip the 53th sunday of some years
sundayInYear 1 = filter (\x -> mod x 7 == 0) [1..365]
sundayInYear n = map (+ (-n + 1)) (sundayInYear 1)

-- 
-- intersect sundayInYear with normal/leap sundays, I have:
firstMSunday leap 1 = if leap then (2, 3) else (2, 2)
firstMSunday leap 2 = if leap then (1, 4) else (2, 3)
firstMSunday leap 3 = if leap then (2, 5) else (1, 4)
firstMSunday leap 4 = if leap then (2, 6) else (3, 5)
firstMSunday leap 5 = if leap then (1, 7) else (1, 6)
firstMSunday leap 6 = if leap then (1, 1) else (1, 7)
firstMSunday leap 7 = if leap then (3, 2) else (2, 1)
firstMSunday _ _ = (0,0)    

--counting the first-month sundays of a range of years
sundayCount _ [] = []
sundayCount (c, t) (x:xs) = z : sundayCount z xs
    where z = firstMSunday (isLeap x) t
--result
problem19 = sum $ [i | (i, _) <- sundayCount (0,2) [1901..2000]]