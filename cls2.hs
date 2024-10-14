-- Hello world!
greeting :: String -> String -> String -> String
greeting x y z = x ++ y ++ z 

-- main = print ( greeting "hello" " " "world" ) -- "hello world"

---------------------------------------------------------------------------------------
---- Define a function maxi with two arguments that delivers the maximum of the two.

maxi :: Int -> Int -> Int
maxi x y 
    | x > y = x
    | otherwise = y


-- main = print ( max 34 56 ) -- 56
-- main = print ( maxi 34 34) -- 56

---------------------------------------------------------------------------------------
---- Triple a number.

triple :: Int -> Int
triple x = x*3

-- main = print ( triple 5 ) -- 15

---------------------------------------------------------------------------------------
---- Compute the cube of a number.

cube :: Int -> Int
cube x = x ^ 3


-- main = print ( cube 4 )  -- 64
-- main = print ( cube 8  ) -- 512

---------------------------------------------------------------------------------------
---- Check if a number is the sum of two other given numbers in any order.
    
issum :: Int -> Int -> Int -> Bool
issum x y z = ( x == y + z)


-- main = print ( issum 10 6 3 )  -- False
-- main = print ( issum 10 6 4 )  -- True

---------------------------------------------------------------------------------------
---- Check if a number is odd. -- odd or even

isoddnr :: Int -> Bool
isoddnr x 
    | odd x = True
    | otherwise = False

    
-- main = print (isoddnr 5) -- True
-- main = print (odd 6) -- False

---------------------------------------------------------------------------------------
---- Check if a number is multiple of 10.

ismult10 :: Int -> Bool
ismult10 x = x `mod` 10 == 0 

-- main = print ( ismult10 20 ) -- True
-- main = print ( ismult10 201 ) -- False

---------------------------------------------------------------------------------------
---- Write a function which returns true if a is divisible by b. 

divBy :: Int -> Int -> Bool
divBy 0 0 = error "Division by 0 not posible"
divBy a b 
    | a`mod`b == 0 = True
    |otherwise = False

-- divBy x y 
--     | y == 0 = error "Division by 0"
--     | otherwise = x `mod` y == 0

-- main = print (divBy 10 2) -- True
-- main = print (divBy 10 3) -- False
-- main = print (divBy 10 0) -- "Dvision by 0"

-- Difference between mod and rem
-- (-7) mod 3 returns 2 because the result takes the sign of the divisor (3), which is positive.
-- (-7) rem 3 returns -1 because the result takes the sign of the dividend (-7), which is negative.

---------------------------------------------------------------------------------------
---- Write a function which returns true if a is divisible by b or vice versa. Fill in the blanks

divAny :: Int -> Int -> Bool
divAny a b = a `div` b ==  0 || b `div` a == 0

-- main = print (divAny 10 20) -- True

---------------------------------------------------------------------------------------
---- Given three integer numbers a, b and c, check if both a and b have the same remainder when divided by c.

sameRem :: Int -> Int -> Int -> Bool
sameRem a b c = a `rem` c == b `rem` c 

-- main = print ( sameRem 12 4 4) -- True
-- main = print ( sameRem 12 4 3) -- False
-- main = print ( sameRem 13 4 3) -- True

---------------------------------------------------------------------------------------
---- Given two integers and a boolean value, check if the first integer is even, the second divisible by 13 and the boolean value is True.
-- Fill in the missing boolean operators.

check :: Int -> Int -> Bool -> Bool
check a b c = even a && b `mod` 13 == 0 && c

-- main = print ( check 4 26 True) -- True
-- main = print ( check 5 26 True) -- False
-- main = print ( check 5 23 True) -- False

--Recursion is a function that calls itself. We are trying to break questions into small parts.
---------------------------------------------------------------------------------------
---- Write a function that takes two arguments, say n and x, and computes their power,
-- in 2 versions - with recursion and without recursion.

power :: Int -> Int -> Int
power n x = n^x

-- main = print ( power 2 5 ) -- 32

powerrec :: Int -> Int -> Int
powerrec n x 
    | x == 0 = 1
    | otherwise = n * powerrec n (x-1) 

-- powerrec n x 
--     | x == 0 = 1
--     | otherwise = n * powerrec n (x-1)

-- n == 2, x == 3
-- 2 * powerrec 2 2 -- n == 2, x == 2
-- 2 * 2 * powerrec 2 1 -- n == 2, x == 1
-- 2 * 2 * 2 * powerrec 2 0 -- n == 2, x == 0
-- 2 * 2 * 2 * 1

-- main = print ( powerrec 2 0) --  1
-- main = print ( powerrec 2 4 ) -- 16

---------------------------------------------------------------------------------------
---- Write a function which calculates the sum of the digits of a number.

digitSum :: Int -> Int
digitSum x
    | x == 0 = x
    | otherwise = x `mod` 10 + digitSum (x `div` 10)

-- main = print (digitSum 1234) -- 10

{-

digitSum 1234 (x == 1234)
(1234 `mod` 10) + digitSum (1234 `div` 10)
4 + (digitSum 123)
4 + 3 + 2 + 1 = 10

digitSum 123 (x == 123)
123 `mod` 10 + digitSum (123 `div` 10)
3 + (digitSum 12)
3 + 2 + 1

digitSum 12 (x == 12)
12 `mod` 10 + digitSum (12 `div` 10)
2 + (digitSum 1)
2 + 1

digitSum 1 (x == 1)
1

-}

-- main = print (123 `mod` 10) -- 123 == 120 + 3
-- main = print (12 `div` 10) -- 12

---------------------------------------------------------------------------------------
---- Write a function sumpowers that takes an integer n and returns the result of powering 
-- n by itself n, then decreasing n by 1 after each powering until n reaches 1.

sumpowers :: Int -> Int
sumpowers n
    | n <= 0 = error "Negative number"
    | n == 1 = 1
    | otherwise = n^n + sumpowers (n-1)


-- main = print (1^1 + 2^2 + 3^3 + 4^4 + 5^5)
-- 1^1 + 2^2 + 3^3 + 4^4 + 5^5
-- main = print (sumpowers 5) -- 3413
-- main = print (sumpowers (-34)) -- Negative number
-- main = print (sumpowers 9) -- 405071317

---------------------------------------------------------------------------------------
---- Sum of squares
-- Compute the sum of the squares of numbers from 0 to n.

squareSum :: Int -> Int
squareSum n 
    | n == 0 = 0
    | otherwise = n^2 + squareSum (n-1) 
-- Examples
-- 1 + 2^2 + 3^2 + 4^2 + 5^2 == 1 + 4 + 9 + 16 + 25 
-- main = print (squareSum 5) -- 55
-- main = print (squareSum 0) -- 0
-- main = print (squareSum 100) -- 338350

---------------------------------------------------------------------------------------
---- Given a positive integer, find the sum of the odd numbers up to that number starting from 1.

sumOdd :: Int -> Int
sumOdd x
    | x == 0 = 0
    | odd x = x + (sumOdd (x-1))
    | otherwise = sumOdd (x-1)

-- main = print $ sumOdd 5 -- 9 
-- main = print $ sumOdd 21 -- 121
-- main = print $ sumOdd 10 -- 25 = 9+7+5+3+1
-- main = print (sumOdd (-13)) -- n has to be positive

---------------------------------------------------------------------------------------
---- Compute for a given positive n the sum of 2i*(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.

--f :: Int -> Int

-- main = print ( f 0 ) -- 0
-- main = print ( f 3 ) -- 68

---------------------------------------------------------------------------------------
---- Write GetLastPositive function
-- Returns the number decreased by the last digit if positive, otherwise returns -1.

getLastPositive :: Int -> Int
getLastPositive x 
    | x < 0 = -1
    | otherwise = x - (x `mod` 10)

-- main = print ( getLastPositive 5856)   -- 5850  
-- main = print ( getLastPositive 689255) -- 689250
-- main = print ( getLastPositive 0)      -- 0
-- main = print ( getLastPositive 8)      -- 0
-- main = print ( getLastPositive (-8554)) -- -1

---------------------------------------------------------------------------------------
---- Convert digit to string
-- Convert an integer from 0 to 3 into a word, otherwise return "Not less or equal to 5".

digitToString :: Int -> String
digitToString x 
    | x == 0 = "Zero"
    | x == 1 = "One"
    | x == 2 = "Two"
    | x == 3 = "Three"
    | otherwise = "Not less or equal to 5"


-- main = print ( digitToString 3) -- "Three"
-- main = print ( digitToString 8) -- "Not less or equal to 5"
-- main = print ( digitToString (-1)) -- "Not less or equal to 5"

---------------------------------------------------------------------------------------
---- Average of 5 numbers
-- Compute the average of 5 numbers.

av5 :: Int -> Int -> Int -> Int -> Int -> Double
av5 a b c d e = fromIntegral (a + b + c + d + e) / 5.0

-- main = print ( av5 1 2 3 4 5) -- 3.0
-- main = print (av5 3 5 7 9 10) -- 6.8

---------------------------------------------------------------------------------------
---- Odd-even operation
-- Return the product if both numbers are odd, sum if both are even, otherwise return 0.

oddEven :: Int -> Int -> Int
oddEven x y 
    | odd x && odd y = x*y
    | even x && even y = x+y
    | otherwise= 0 


-- main = print (oddEven 474 8983) -- 0
-- main = print (oddEven 6 6) -- 12
-- main = print (oddEven 7 7) -- 49

---------------------------------------------------------------------------------------
---- Are numbers sorted?
-- Check if 5 numbers are sorted in increasing order.

isSorted :: Int -> Int -> Int -> Int -> Int -> Bool
isSorted a b c d e = a <= b && b <= c  && c <= d && d <= e

-- main = print (isSorted 1 1 1 1 1) -- True
-- main = print (isSorted 1 2 3 4 5) -- True
-- main = print (isSorted 4 3 2 1 0) -- False

---------------------------------------------------------------------------------------
---- Transform days into years, weeks, and days.
-- Convert the number of days into a string of years, weeks, and days.

--transform :: Int -> String

-- main = print (transform 375) -- "1 year 1 week 3 days"
-- main = print (transform 365) -- "1 year 0 week 0 days"
-- main = print (transform 1050) -- "2 year 45 week 5 days"
-- main = print (transform 2500) -- "6 year 44 week 2 days"

---------------------------------------------------------------------------------------
---- Armstrong number
--  If sum of cubes of each digit of the number is equal to the number itself, then the number is called an   Armstrong number.
--  153 = 1^3 + 5^3 + 3^3
--  Given a positive integer number, write a function to determine whether it is an Armstrong number or not.

armstrong :: Int -> Bool
armstrong x  = ((x `div` 100) ^ 3 + (((x `div` 10)  `mod` 10) ^ 3) + (x `mod` 10) ^ 3) == x  

-- main = print ( armstrong 153) -- True
-- main = print ( armstrong 370) -- True
-- main = print ( armstrong 0) -- True
main = print ( armstrong 12) -- False
