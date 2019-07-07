-- The issue was found in a code challenge, part of Christina's recruitment process.

module OverlapBooking where

-- A. Logical Question

-- Let assume that we have 2 pairs of bookings: (A, B) and (C, D) considering that A < B, C < D
-- In general, the two bookings are NOT overlap each other only if (B < C) or (D < A)
-- Otherwise, the bookings overlap


-- B. Expansion Question

-- In my opinion, it's more efficient to do a sort_by_first_date on list of bookings first.
-- And remove duplicated elements on sorting phase can also help detect overlap sooner.  
-- Then I can verify overlapped bookings pair-by-pair by traversing the sorted list easily.


-- C. Implementation

-- A customized merge sort, that also removes duplicated bookings. 
-- O(nlogn) 
-- example: bsort [(4,6), (5,7), (7,10), (6,6), (1,3), (4,5)]
-- result:        [(1,3), (4,6), (5,7), (6,6), (7,10)]
bsort (x:[])   = [x] -- in case there are no more booking to process
bsort xs       = merge (bsort l) (bsort r) where
    (l, r)     = splitAt (div (length xs) 2) xs
    -- do merge the left and right parts
    merge x [] = x
    merge [] y = y
    merge x y                                                                                                                                               
        | (fst $ head x) < (fst $ head y) = head x : (merge (tail x) y)
        | (fst $ head x) > (fst $ head y) = head y : (merge x (tail y))
        | otherwise      = merge x (tail y) -- remove the element appears twice

-- Verify if the list has overlapped bookings or not
-- Return True immediately, if the length of sorted list is not equal to original list
-- If the lengths are equal, do compares recursively
-- O(1) - O(n)
has_overlap xs = if length xs /= length sorted then True else comp sorted
    where sorted          = bsort xs
          comp (b1:b2:[]) = (snd b1 >= fst b2) -- in case there are no more bookings to process
          -- return False if overlap is detected, otherwise recursive do next compare
          comp (b1:b2:bs) = if (snd b1 >= fst b2) then True else comp (b2:bs) 


-- D. Alternatives (an idea to use no-sql)

-- By using Map (key-value like Data Structure)
-- A Key should be a room:id#date
-- Value should be bookings of that room, in form of a list/array
-- Ex: k: room#06:06062019, v: [booking#2, booking#4, booking#6]

-- So each time a new booking (06 -> 08 Jun) needs a placement, to a room 06
-- Select all days in the range of that booking
-- Ex: find(room:06#06062019)
--     find(room:06#06072019)
--     find(room:06#06082019)
--     if all Empty, so the booking on room 06 is possible

-- E. Relate to interview question

-- Keep 8 records of booking type: 01bed, 02bed .. 08bed
-- Each record: (01bed, [book#2, book#4, book#6]), (02bed, [book#3, book#5, book#7])...
-- And Zip the two records (01-07) (02-06)... together

-- Atm, my solution during interview is sort then pair (first-last).


