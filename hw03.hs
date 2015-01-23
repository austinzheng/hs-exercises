-- Austin Zheng
-- Problem Set 3

{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Given a list of n items, return the counting-up list from 1 to n.
cu :: [a] -> [Int]
cu x = [1..length x]

-- Exercise 1
-- Given a list with n items, produce a list of n lists where list i contains every i-th element.
skips :: [a] -> [[a]]
skips x = map (\v -> ev x v) (cu x)

-- How it works:
-- 1. We create a vector of indexes from 1 to n. For example, if we have 3 items, we create [1,2,3].
-- 2. Each item in the vector, 'z' is mapped to a filtered list of items where only every 'z-th' item is included.
-- 3. We return the list of lists.

-- Given a list and an index 'n', return a list containing every n'th item
ev :: [a] -> Int -> [a]
ev x n = map (\b -> x !! (b - 1)) (filter (\a -> mod a n == 0) (cu x))

-- How 'ev' works:
-- 1. Overall, what we're doing is creating a list of 1-based indexes we want (e.g. [2,4,6]), and then mapping each
--    index to the corresponding item in the input array.
-- 2. To generate our list of indexes, we use a range (through the 'cu' function).
-- 3. The list comprehension first produces a list of indexes from 1 to the list length.
-- 4. We then filter the index list by taking the modulus of each index and the value of n. This gives us the indexes
--    for every 'n-th' item (1-based).
-- 5. Our map function takes each 1-based index 'b', and maps it to the list item (e.g. x[b-1]). The '(x - 1)' converts
--    the 1-based index back to a 0-based index.


-- Exercise 2
-- Given a list of integers, return a list of the local maxima.
localMaxima :: [Integer] -> [Integer]
localMaxima x = map (\a -> x !! a) (filter (\a -> ismx a x) (cu x))

-- How it works:
-- 1. The 'filter' portion takes a list of all indexes and returns a list of the indexes corresponding to local
--    maxima.
-- 2. The 'map' portion takes each local maximum index and maps it to the actual maximum.

-- Given an index and a list, determine whether the item at that index is a local maximum
ismx :: Int -> [Integer] -> Bool
ismx n x = n > 0 && n < length x - 1 && x !! (n - 1) < x !! n && x !! n > x !! (n + 1) 

-- How 'ismx' works:
-- A number at the index 'n' is a local maximum if:
-- 1. n > 0 (not the first element)
-- 2. n < (length x) - 1 (not the last element)
-- 3. x[n] > x[n-1]
-- 4. x[n] > x[n+1]


-- Exercise 3
-- Given a list of numbers between 0 and 9 (inclusive), return a text histogram of the number frequencies.
histogram :: [Integer] -> String
histogram a = build (enum a) ++ "==========\n0123456789\n" 

-- How it works:
-- 1. We use 'enum' to get the frequencies of the digits...
-- 2. ...then use 'build' to turn the frequencies list into a histogram.
-- 3. Finally, we append the legend.

-- Given a row index of the histogram and a list that describes frequencies of digits, return the string for that
-- histogram row.
toRow :: Int -> [Int] -> String
toRow a x = map (\v -> if v >= a then '*' else ' ') x

-- How 'toRow' works:
-- 1. Our 'a' input is a row in the histogram. For example, if 'a' is 5, then we are building the string for the fifth
--    row of the histogram.
-- 2. We can transform the frequency list into the string. Each frequency in the list gets mapped to a character in the
--    string. Each character in the string, from left to right, corresponds with a digit from 0 to 9.
-- 3. This means that any digit that shows up 5 or more times should get a '*' in its slot in the string, otherwise a 
--    space. 

-- Given a list that describes frequencies of digits, return the histogram string for that list.
build :: [Int] -> String
build x = unlines (map (\a -> toRow a x) (reverse [1..maximum x]))

-- How 'build' works:
-- 1. First, we use 'reverse [1..maximum x]' to build up a list where each item corresponds to a row in the histogram.
--    The value of the item is the y-axis tick mark (e.g. '9' means the 9th row up from the legend). The list is
--    reversed because the lowest numbers are at the bottom of the histogram (e.g. they come last).
-- 2. For each row in the histogram we use 'toRow' to generate the corresponding string for that row.
-- 3. Finally, we use unlines to turn our lists of strings into a single string, with each row on its own line.

-- Given a list of numbers, return a list of the frequencies of each digit from 0 to 9
enum :: [Integer] -> [Int]
enum x = map (\a -> length (filter (==a) x)) [0..9]

-- How 'enum' works:
-- 1. For each digit between 0 and 9...
-- 2. ...we filter the input list x based on that digit, and then get the length of the filtered length.
-- 3. This gives us the number of times each digit appears.
