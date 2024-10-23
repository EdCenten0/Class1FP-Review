{- Progress Task -}

{- Your neptun code : vu2gxc -}

{-
	Given a 2 dimensional list of integer, remove middle element of each sublist.
	
	Note:
	[1,2,3,4,5] -> [1,2,4,5]
	[1,2,3,4,5,6] -> [1,2,4,5,6] or [1,2,3,5,6] (both are correct)
-}

removeSubMid :: [[Int]] -> [[Int]]
removeSubMid [] = []
removeSubMid (x:xs)
    | length x /= 0 =  drop (length x `div` 2)  x : xs

    -- something = xs

-- main = print (removeSubMid []) -- []
main = print (removeSubMid [[1,2,3,4,5,6,7,8], [1,2,3,4,5], [3], [3,4], []])  -- [[1,2,3,4,6,7,8],[1,2,4,5],[],[3],[]]
-- main = print (removeSubMid [[3], [5], [9, 10], []]) -- Output: [[],[],[9],[]]
