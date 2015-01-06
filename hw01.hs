-- Austin Zheng
-- Problem Set 1

-- Exercise 1: defining toDigits and toDigitsRev

-- Given a number, return a list of its digits, reversed.
-- e.g. 12345 -> [5,4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

-- Given a number, return a list of its digits
-- e.g. 12345 -> [1,2,3,4,5]
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


-- Exercise 2: defining doubleEveryOther

-- Given a list of numbers and whether or not to double the first number, double every other number
doubleHelper :: [Integer] -> Bool -> [Integer]
doubleHelper [] _ = []
doubleHelper (n:xn) True = (n * 2) : doubleHelper xn False
doubleHelper (n:xn) False = n : doubleHelper xn True

-- Given a list of numbers, double every other number (with the last number remaining untouched)
-- e.g. [1,2,3,4] -> [2,2,6,4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n
  | (length n) `mod` 2 == 0 = doubleHelper n True
  | otherwise = doubleHelper n False


-- Exercise 3: defining sumDigits

-- Given a single integer, return the sum of its digits
singleSumDigits :: Integer -> Integer
singleSumDigits n = foldl (\m n -> m + n) 0 (toDigitsRev n)

-- Given a list of numbers, calculate the sum of all the digits of all numbers
-- e.g. [1,23,4,56] -> 21 (by way of 1+2+3+4+5+6)
sumDigits :: [Integer] -> Integer
sumDigits n = foldl (\a b -> a + singleSumDigits b) 0 n


-- Exercise 4: defining validate

-- Given a credit card number, return whether or not it validates properly
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0


-- Exercise 5: Towers of Hanoi
-- I hate this puzzle

type Peg = String
-- A Move represents a Hanoi action where the top disc from the first Peg is moved to the second Peg
type Move = (Peg, Peg)
-- Given the number of discs and three names for pegs, output a list of all moves necessary to move n discs from the
-- first peg to the second peg
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest temp = [(source, dest)]
hanoi n source dest temp = (hanoi (n - 1) source temp dest) ++ (hanoi 1 source dest temp) ++ (hanoi (n - 1) temp source dest)
