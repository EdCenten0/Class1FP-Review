---------------------------------------------------------------------------------------
---- The f function will take a positive integer n and return the sum of (3i + 4)*(i + 5) for i from 1 to n .
-- If n is 0, it should return 0.

-- f :: Int -> Int

-- main = print (f 3) -- 216
-- main = print (f 1) -- 42
-- main = print (f 0) -- 0
-- main = print (f 6) -- 792

---------------------------------------------------------------------------------------
{-
        Overview of lists:
            1. Homogeneity: All elements of list must be of the same type.
            2. Basic functions:
                1. "head list" retrieves the first element.
                2. "tail list" returns all but the first element.
            3. Use "++" to combine two lists.
            4. "length list" gives the number of elements of list
            5. Nested Lists: Lists can contain other lists.
            6. Indexing: Access elements with (list !! index).

            for more information check: https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html


-}


--Generate 2 digits number starting with 9
-- main = print [90..99]


--Generate multiples of 5 in the range of 0 to 100
-- main = print [0, 5..100]


--Generate all the 1-digit negatives
-- main = print [(-1)..(-100)]


--Generate an empty list   
-- main = print [3..1]


-- Generate positive even numbers from 0 up to 100
-- main = print[0,2..100]
       

---------------------------------------------------------------------------------------
-- Basic list functions - review

-- main = print (head [101, 20, 33, 43, 51] )    -- 101 first element of the list

-- main = print (head [ [101, 20], [33, 43, 51]] )  -- [101,20]

-- main = print (head [[[101, 20]], [[33]],[[ 43, 51], [1,2,3]]] )  -- [[101,20]]

-- main = print (tail [10, 22, 32, 43, 58] )  -- [22,32,43,58] everything except first element as a list

-- main = print (drop 5 [1, 2, 3, 4, 5, 6, 7] ) -- [6,7] delete first 5 elements

-- main = print (take 4 [1, 2, 3, 4, 5] ) -- [1,2,3,4] take first 4 elements

-- main = print ([1..10] ++ [10, 9..1] )  -- [1,2,3,4,5,6,7,8,9,10,8,88] concatenation, appends the second to the first

-- main = print (reverse [1..8])  -- [8,7,6,5,4,3,2,1] reverses a list

-- main = print (length [[[101, 20]], [[33]],[[ 43, 51], [1,2,3]]] )   -- 100 number of elements

-- main = print (last [100, 200, 300])   -- 300 last list element

-- main = print (init [100, 200, 300])  -- [100,200] all except the last          

-- main = print ( elem 2 [0..22] ) -- True, check membership

-- main = print ( elem 5 [10..20]) -- False

-- main = print (concat [[1,2,3,4], [5], [6,7,8]] ) -- [1,2,3,4,5,6,7,8] flattens a list of lists

---------------------------------------------------------------------------------------
---- Define two lists. First one should include first 5 positive integers. 
-- Do this by writing them one by one.
-- Second one should include first 101 positive integers. Do this by using .. notation

first :: [Int]
-- first = [1,2,3,4,5]
first = [1..101]



-- main = print first

second :: [Int]
second = [1..101]

-- main = print second

---------------------------------------------------------------------------------------
---- Define a list of three strings: "apple", "banana", and "cherry". 
-- Write a function firstFruit that returns the first string from a list,
-- and restFruits function returns all the values except first.
-- remark: head [] and tail [] do not return normally (throw an exception)

fruits :: [String]
fruits = ["apple", "banana", "cherry"]

firstFruit :: [String] -> String
firstFruit x = head x


-- main = print (firstFruit ["apple", "banana", "cherry"]) -- "apple"
-- main = print (firstFruit ["orange"]) -- "orange"
-- main = print (firstFruit ["kiwi", "mango"]) -- "kiwi"

restFruits :: [String] -> [String]
restFruits x = tail x


-- main = print (restFruits ["apple", "banana", "cherry"]) -- ["banana", "cherry"]
-- main = print (restFruits ["orange"]) -- []
-- main = print (restFruits ["kiwi", "mango"]) -- ["mango"]

---------------------------------------------------------------------------------------
---- Write a function that puts the first value of the list as the last one, 
-- i.e. rotates once to the right.
-- Ex : [1,2,3] -> [2,3,1]

transformList :: [a] -> [a]
transformList list = tail list ++ [head list]
-- main = print (transformList ["a", "b", "c"]) -- ["b", "c", "a"]
-- main = print (transformList [True, False, True]) -- [False, True, True]
-- main = print (transformList [1]) -- [1]

{-
   The following line calls transformList with an empty list.
   Since Haskell cannot infer the type of an empty list ([]), 
   we provide a type annotation (e.g., [Int]) to resolve the ambiguity.
-}

--main = print (transformList ([] :: [Int])) -- empty list


---------------------------------------------------------------------------------------
----The concatTails function takes two input lists (with length at least 2), 
--removes the first elements from each, and 
--concatenates the remaining lists. 

concatTails :: [a] -> [a] -> [a]
concatTails x y = drop 1 (x) ++ drop 1 (y)


-- main = print (concatTails [1, 2, 3] [4, 5, 6]) -- [2, 3, 5, 6]
-- main = print (concatTails ["a", "b", "c"] ["d", "e", "f"]) -- ["b", "c", "e", "f"]
-- main = print (concatTails [True, False] [False, True]) -- [False, True]
-- main = print (concatTails [1] []) -- "input too short"

---------------------------------------------------------------------------------------
----The concatHeads function takes the heads of both lists and returns them as a new list. 
-- Do error handling using null built-in function for empty lists.

concatHeads :: [a] -> [a] -> [a]
concatHeads x y = [head x] ++ [head y]
    

-- main = print (concatHeads [1, 2, 3] [4, 5, 6]) -- [1, 4]
-- main = print (concatHeads ["a", "b", "c"] ["d", "e", "f"]) -- ["a", "d"]
-- main = print (concatHeads [True, False] [False, True]) -- [True, False]
-- main = print (concatHeads ([] :: [Int]) ([] :: [Int]) ) -- Null list

---------------------------------------------------------------------------------------
---- The averageList function takes a list of integers and returns the average of the list as a float.
-- If the list is empty, it should return 0.
-- Note: You can use fromIntegral built in function in order to convert Int to Float

averageList :: [Int] -> Float
averageList x= fromIntegral (sum x) / fromIntegral (length x)


-- main = print (averageList [1, 2, 3, 4, 5]) -- 3.0
-- main = print (averageList [10, 20, 30]) -- 20.0
-- main = print (averageList [7, 14, 21, 28]) -- 17.5
-- main = print (averageList []) -- 0.0

---------------------------------------------------------------------------------------
---- The firstLast function takes a list and returns a new list containing 
-- the first and last elements of the input list. Assume the list has at least one element.

firstLast :: [a] -> [a]
firtsLast [] = []
firstLast [x] = [x]
firstLast (x:xs) = [x] ++ [last xs]

-- main = print (firstLast "hello") -- "ho"
-- main = print (firstLast [1,2,3,4]) -- [1,4]
-- main = print (firstLast ["cad","adsa","fsafas","aa"]) -- ["cad","aa"]
-- main = print (firstLast [1]) -- [1,1]

---------------------------------------------------------------------------------------
---- The concatTailAndRest function takes a list and returns a new list by concatenating 
-- the tail of the list with the list excluding the last element. Assume the list is not empty.

concatTailAndRest :: [a] -> [a]
concatTailAndRest [] = []
concatTailAndRest (x:xs) = xs ++  [x] ++ init xs


-- main = print (concatTailAndRest [])
-- main = print (concatTailAndRest [1, 2, 3, 4, 5]) -- [2,3,4,5,1,2,3,4]
-- main = print (concatTailAndRest ["a", "b", "c"]) -- ["b", "c", "a", "b"]
-- main = print (concatTailAndRest [True, False, True]) -- [False, True, True, False]

---------------------------------------------------------------------------------------
---- The sim function takes a list of integers and returns True if the list is symmetrical, otherwise False.
-- If the list is empty, it should return True.

sim :: [Int] -> Bool
sim [] = True
sim [x] = True
sim (x:y:xs) 
    | x == last xs = True
    | x == last xs && y == head xs = True
    | otherwise = False


-- main = print (sim [1, 2, 1]) -- True
-- main = print (sim [1, 2, 3, 4, 5]) -- False
-- main = print (sim [1, 2, 2, 1]) -- True
-- main = print (sim []) -- True
-- main = print (sim [1]) -- True

---------------------------------------------------------------------------------------
---- The middle function takes a non-empty list of integers and returns the middle element.
-- If the list is empty, it should return an error message.

middle :: [Int] -> Int
middle [] = error "The list is empty"
middle list = list !! (length list `div` 2)


-- main = print (middle [1..5]) -- 3
-- main = print (middle [1..4]) -- 3
-- main = print (middle []) -- Your list is empty

---------------------------------------------------------------------------------------
---- The cut function takes a list of integers and return a list of two lists, 
-- splitting the original list at the middle.
-- If the list is empty, it should return two empty lists.

cut :: [Int] -> [[Int]]
cut [] = [[],[]]
cut list = [part1, part2]
    where
        (part1, part2) = splitAt (length list `div` 2) list

-- main = print (cut [1..10]) -- [[1,2,3,4,5],[6,7,8,9,10]]
-- main = print (cut [1..11]) -- [[1,2,3,4,5],[6,7,8,9,10,11]]
-- main = print (cut []) -- [[],[]]
-- main = print (cut [1]) -- [[],[1]]

---------------------------------------------------------------------------------------
---- The f1 function takes a list of integers and returns a new list 
-- with 3 added to every element using recursion.

f1 :: [Int] -> [Int]
f1 [] = []
f1 (x:xs) 
    | length (x:xs) == 0 = (x:xs)
    | otherwise = [x+3] ++ f1 xs


-- main = print (f1 [1, 5, 3, 1, 6]) -- [4, 8, 6, 4, 9]
-- main = print (f1 [0, -3, 7]) -- [3, 0, 10]
-- main = print (f1 []) -- []
-- main = print (f1 [10]) -- [13]

---------------------------------------------------------------------------------------
---- The f2 function will take a list of integers 
-- and returns a new list with the double of the positive elements.

f2 :: [Int] -> [Int]
f2 [] = []
f2 (x:xs) 
    | length (x:xs) == 0 = (x:xs)
    | x <= 0 = f2 xs 
    | otherwise =  [x*2] ++ f2 xs


-- main = print (f2 [1, 2, -2, 3, -4]) -- [2, 4, 6]
-- main = print (f2 [0, -3, 7]) -- [14]
-- main = print (f2 []) -- []

---------------------------------------------------------------------------------------
---- The sq function will take a list of integers and returns a new list with the square of each element.

sq :: [Int] -> [Int]
sq [] = []
sq (x:xs) 
    | length (x:xs) == 0 = (x:xs)
    | otherwise = [x*x] ++ sq xs


-- main = print (sq [1..5]) -- [1, 4, 9, 16, 25]
-- main = print (sq [0, -3, 7]) -- [0, 9, 49]
-- main = print (sq []) -- []
-- main = print (sq [10]) -- [100]

---------------------------------------------------------------------------------------
---- The f4 function will take a list of lists of integers and returns a new 
-- list of lists with the square of every element.

f4 :: [[Int]] -> [[Int]]
f4 [] = []
f4 (x:xs) = sq x : f4 xs
 

-- main = print (f4 [[1,2],[3,4,5,6],[7,8]]) -- [[1,4],[9,16,25,36],[49,64]]
-- main = print (f4 [[], [1], [2, 3]]) -- [[], [1], [4, 9]]
-- main = print (f4 []) -- []
-- main = print (f4 [[0, -1, -2]]) -- [[0, 1, 4]]

---------------------------------------------------------------------------------------
---- The not_five function will take a list of integers and returns a new list 
-- with all elements equal to 5 removed.
-- This function should not use higher-order functions.

not_five :: [Int] -> [Int]
not_five [] = []
not_five (x:xs) 
    | length (x:xs) == 0 = []
    | x /= 5 = [x] ++ not_five xs
    | otherwise = not_five xs


-- main = print (not_five [5, 4, 5, 4, 3]) -- [4, 4, 3]
-- main = print (not_five [5, 5, 5]) -- []
-- main = print (not_five [1, 2, 3, 4]) -- [1, 2, 3, 4]
-- main = print (not_five []) -- []
-- main = print (not_five [5]) -- []


not_five2 :: [Int] -> [Int]
not_five2 [] = []
not_five2 list = filter (\x -> x /= 5) list


-- main = print (not_five2 [5, 4, 5, 4, 3]) -- [4, 4, 3]
-- main = print (not_five2 [1, 2, 3, 4]) -- [1, 2, 3, 4]

---------------------------------------------------------------------------------------
---- The del function will take an integer n and a list of integers, 
-- and returns a new list with all occurrences of n removed.

del :: Int -> [Int] -> [Int]
del _ [] = []
del n (x:xs) 
    | length (x:xs) == 0 = []
    | x /= n = [x] ++ del n xs
    | otherwise = del n (xs)


-- main = print (del 5 [1, 5, 6, 7, 5, 8, 5]) -- [1, 6, 7, 8]
-- main = print (del 3 [1, 2, 3, 4, 3, 5]) -- [1, 2, 4, 5]
-- main = print (del 0 [0, 0, 0, 0]) -- []
-- main = print (del 1 [2, 3, 4]) -- [2, 3, 4]
-- main = print (del 7 []) -- []


---------------------------------------------------------------------------------------
-- Insert 0 in front of every sublist of a list.
-- E.g. for [[1, 2], [3, 4], [5]] the result is [[0,1,2],[0,3,4],[0,5]]


insertZero :: [[Int]] -> [[Int]]
insertZero [] = []
insertZero (x:xs)
    | length (x:xs) == 0 = []
    | otherwise = [[0] ++ x] ++ insertZero xs


-- main = print (insertZero [[1, 2], [3, 4], [5]]) -- [[0,1,2],[0,3,4],[0,5]]
-- main = print (insertZero [[]]) -- [[0]]
-- main = print (insertZero []) -- []
-- main = print (insertZero [[1]]) -- [[0,1]]


---------------------------------------------------------------------------------------
---- The productf function takes a list of integers and returns the product of all the elements.
-- If the list is empty, it should return 1 (the identity for multiplication).

-- the built in function is product
-- main = print (product [1..7]) -- 5040

productf :: [Int] -> Int
productf [] = 1
productf list = product list


-- main = print ( a  [1..5]) -- 120
-- main = print (productf [1, 2, 3, 4, 5]) -- 120
-- main = print (productf [10, 20, 30]) -- 6000
-- main = print (productf []) -- 1
-- main = print (productf [7]) -- 7

---------------------------------------------------------------------------------------
---- Write a function filterEven that takes a list of integers and returns a new list containing only the even numbers.

filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
    | even x = x : filterEven xs
    | otherwise = filterEven xs

-- filterEven :: [Int] -> [Int]
-- filterEven list = filter (\x -> even x ) list

-- main = print (filterEven [1, 2, 3, 4, 5, 6]) -- [2, 4, 6]
-- main = print (filterEven [7, 9, 13]) -- []
-- main = print (filterEven [0, -2, 3, -4]) -- [0, -2, -4]
-- main = print (filterEven []) -- []

---------------------------------------------------------------------------------------
---- Write a function wordLengths that takes a list of strings and 
-- returns a list of integers representing the length of each word.

wordLengths :: [String] -> [Int]
wordLengths [] = []
wordLengths (x:xs) = [length x] ++ wordLengths xs


-- main = print (wordLengths ["apple", "banana", "cherry"]) -- [5, 6, 6]
-- main = print (wordLengths ["hello", "world"]) -- [5, 5]
-- main = print (wordLengths ["a", "abc", ""]) -- [1, 3, 0]
-- main = print (wordLengths []) -- []

---------------------------------------------------------------------------------------
-- Write a function that removes the first and last element of a list. 
-- Assume at least 1 element is given.

removeEnds :: [a] -> [a]
removeEnds [] = []
removeEnds list = drop 1 (init list)  


-- main = print (removeEnds [1, 2, 3, 4]) -- [2,3]
-- main = print (removeEnds ["a", "b", "c"]) -- ["b"]
-- main = print (removeEnds [1.1, 3.3]) -- []
-- main = print (removeEnds [True]) -- []

---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list and returns a new list with the elements in reverse order. 
-- If the list is empty, return an empty list.
-- remark: built-in function is reverse

-- reverseList :: [a] -> [a]
-- reverseList [] = []
-- reverseList list = reverse list

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

-- main = print (reverseList [1, 2, 3, 4]) -- [4, 3, 2, 1]
-- main = print (reverseList ["apple", "banana", "cherry"]) -- ["cherry", "banana", "apple"]
-- main = print (reverseList [True, False]) -- [False, True]

---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list of integers and 
-- returns a new list where each odd number is doubled, and each even number is tripled.

processList :: [Int] -> [Int]
processList [] =[]
processList (x:xs) 
    | odd x = [x*2] ++ processList xs
    | otherwise = [x*3] ++ processList xs


-- main = print (processList [1, 2, 3, 4, 5]) -- [2, 6, 6, 12, 10]
-- main = print (processList [10, 15, 20, 25]) -- [30, 30, 60, 50]
-- main = print (processList [0, -3, 4, -5]) -- [0, -6, 12, -10]
-- main = print (processList []) -- []


---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list of lists of integers and 
-- returns a new list containing the count of numbers greater than 3 in each inner list.

countInList :: [Int] -> Int
countInList list = length (filter (\x -> x > 3) list)



countGT3 :: [[Int]] -> [Int]
countGT3 [] = []
countGT3 (x:xs) = [countInList x] ++ countGT3 xs



-- main = print (countGT3 [[1, 2, 3], [4, 5, 6], [2, 3, 4]]) -- [0, 3, 1]
-- main = print (countGT3 [[0, 1, 2], [3], [4, 5]]) -- [0, 0, 2]
-- main = print (countGT3 [[5, 6, 7], [8, 9], []]) -- [3, 2, 0]
-- main = print (countGT3 []) -- []


---------------------------------------------------------------------------------------
-- Write a function elementAt that takes an index n and a list and returns the element at the given index. 
-- The function should give an error message if the index is out of bounds.
-- remark: Lists are 0-indexed.

elementAt :: Int -> [a] -> a
elementAt _ [] = error "Error: Index out of bounds"
elementAt x list 
    | length list > x && x >= 0 =  list !! x
    | otherwise = error "Error: Index out of bounds"


-- main = print( elementAt 2 [1, 2, 3, 4, 5])  -- 3
-- main = print( elementAt 5 [1, 2, 3, 4, 5]) -- Error: Index out of bounds
-- main = print( elementAt 0 [1, 2, 3, 4, 5]) -- 1
-- main = print(elementAt 4 "haskell") -- e
-- main = print (elementAt (-2) [1, 2, 3])  -- Error: Index out of bounds
-- main = print (elementAt 5 [1, 2, 3]) -- Error: Index out of bounds


---------------------------------------------------------------------------------------

-- Write a function sliceList that takes two integers start and end, and a list. 
-- The function should return the sublist that starts at index start and ends at index end.
-- The start index must be non-negative, the end index must not be less than start, 
-- and the end index must not exceed the length of the list. If any of these conditions are violated, 
-- the function should return an error message indicating 'Invalid indices'.

sliceList :: Int -> Int -> [a] -> [a]
sliceList _ _ [] = []
sliceList x y list
    | x >= 0 && y > x && y < length list = [list !! x] ++ take (y-(x+1)) (drop (x+1) list)
    | otherwise = error "Invalid Indices"


-- main = print (sliceList 1 4 [1, 2, 3, 4, 5, 6])  -- [2, 3, 4]
-- main = print (sliceList 0 3 "haskell")          -- "has"
-- main = print (sliceList 2 4 [10, 20, 30, 40, 50]) -- [30, 40]
-- main = print (sliceList 3 1 [1, 2, 3, 4])       -- Error: Invalid indices
-- main = print (sliceList 0 10 [1, 2, 3])         -- Error: Invalid indices



------------------------------------------------------------------------------------------

-- The firstGreaterThanFive function returns the first element greater than 5 in a list of integers. 
-- If there is no greater than 5 return -1.

firstGreaterThanFive :: [Int] -> Int
firstGreaterThanFive [] = -1
firstGreaterThanFive (x:xs)
    | x <= 5 && x > 0 = firstGreaterThanFive xs
    | x <= 0 = -1
    | otherwise = x



-- main = print (firstGreaterThanFive [1, 2, 6, 4, 8]) -- 6
-- main = print (firstGreaterThanFive [1, 2, 3, 4]) -- -1
-- main = print (firstGreaterThanFive [5, 5, 7]) -- 7



--------------------------------------------------------------------------------------------------------


-- The replaceFirst function replaces the first occurrence of an element in the list with a new given value.

replaceFirst :: Int -> Int -> [Int] -> [Int]
replaceFirst _ _ [] = []
replaceFirst n m (x:xs) 
    | x == n = m : xs
    | otherwise = x : replaceFirst n m xs

-- main = print (replaceFirst 3 99 [1, 2, 3, 4, 3]) -- [1, 2, 99, 4, 3]
-- main = print (replaceFirst 5 42 [1, 2, 3, 4]) -- [1, 2, 3, 4] (no change)
-- main = print (replaceFirst 1 0 [1, 1, 1]) -- [0, 1, 1]



-----------------------------------------------------------------------------------

-- Write a function that takes a list of integers and returns a list where:
-- every even number is replaced by the character 'e' and 
-- every odd number is replaced by the character 'o'
-- 0 is replaced by 'x'. 
-- For example, the list [0,1,2,3,4,5,6] should be transformed into ['x','o','e','o','e','o','e'].

replaceInts :: [Int] -> [Char]
replaceInts [] = []
replaceInts (x:xs) 
    | x == 0 = 'x' : replaceInts xs
    | even x = 'e' : replaceInts xs
    | odd x = 'o' : replaceInts xs
    | otherwise =  replaceInts xs
 


-- main = print (replaceInts [0,1,2,3,4,5]) --"xoeoeo"
-- main = print (replaceInts [1,5,3,2,0,3,87,1,2,0]) -- "oooexoooex"



---------------------------------------------------------------------------------------------------

-- The doubleElements function returns a list where even elements are divided by 2 ,and odd element are multiplied by 2.


doubleElements :: [Int] -> [Int]
doubleElements [] = []
doubleElements (x:xs)
    | even x = x `div` 2 : doubleElements xs
    | otherwise = x*2 : doubleElements xs


-- main = print (doubleElements [1, 2, 3]) -- [2, 1, 6]
-- main = print (doubleElements [-1, -2, -3]) -- [-2,-1,-6]
-- main = print (doubleElements [0, 10, 5]) -- [0, 5, 10]
-- main = print (doubleElements []) -- []



