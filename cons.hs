evenList :: [Int] -> [Int]
evenList list = filter (\x -> even x) list

-- main = print(evenList [1,2,3,4,5,6])


recursiveEvenList :: [Int] -> [Int]
recursiveEvenList [] = []
recursiveEvenList (x:xs)
    | even x = x:recursiveEvenList(xs)
    | otherwise = recursiveEvenList xs


-- main = print(recursiveEvenList [1,2,3,4,5,6])

--- Given a list of lists, for each list create a new list which has 2 numbers.
-- The first number represents the number of even numbers in that list.
-- The second number represents the sum of the even numbers in that list.
 

helper  :: [Int] -> [Int]
helper list = [length (filter (\x -> x `mod` 5 == 0) list), sum ((filter (\x -> x `mod` 5 == 0) list))]


cntEvens :: [Int] -> Int
cntEvens [] = 0
cntEvens (x:xs)
    | even x = 1 + cntEvens xs
    | otherwise = cntEvens xs

sumEvens :: [Int] -> Int
sumEvens [] = 0
sumEvens (x:xs) 
    |even x = x + sumEvens xs
    |otherwise = sumEvens xs

countSumEvens :: [[Int]] -> [[Int]]
countSumEvens list = map helper list

-- countSumEvens list = [filter (\x -> even x) map (\x -> even x == true)]
 
 
main = print (countSumEvens [[1,2,3,4],[71,43,42,92,3,8,1,8],[90,2,4,4],[]]) -- [[2,6],[4,150],[4,100],[0,0]]
-- main = print (countSumEvens [[1,3,5,6],[102,104,104,104],[2,2,2],[1,1,1]]) -- [[1,6],[4,414],[3,6],[0,0]]
-- main = print (countSumEvens [[]]) -- [[0,0]]
-- main = print (countSumEvens [[2,4,6],[1,1,1,1],[0,0,0,0]]) -- [[3,12],[0,0],[4,0]]
