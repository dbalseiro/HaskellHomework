import Data.List
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the
-- n th list in the output should contain every n
-- th element from the input list.
-- For example:
-- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []
-- Note that the output should be the same length as the input.
skips :: [a] -> [[a]]
skips [] = []
skips list = map filterIndexedList (mapIndexedList list 0)

data IndexedList a = IndexedList Int [a]
  deriving Show

-- transform a list into a list with index and the rest of the array
-- for instance if i got [1,2,3] it returns (0, [1,2,3]), (1, [2,3]), (2, [3])
mapIndexedList :: [a] -> Int -> [IndexedList a]
mapIndexedList [] _ = []
mapIndexedList list n = (IndexedList n list) : mapIndexedList (drop 1 list) (n+1)

-- transform the array inside the indexed list into a filtered array
filterIndexedList :: IndexedList a -> [a]
filterIndexedList (IndexedList n list) = filterElements list n 0

-- the filter works with the position of the element in the array and the total
-- number of elements to filter the nth element of the array
filterElements [] _ _ = []
filterElements (x:xs) total index
  | total == 0 = x:xs
  | index `mod` (total + 1) == 0 = x : filterElements xs total (index +1)
  | otherwise = filterElements xs total (index + 1)


-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5] , the only local maximum is 4 , since
-- it is greater than the elements immediately before and after it (3 and 1).  5
-- is not a local maximum since there is no element that comes after it.
-- Write a function
-- localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in
-- order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

--localmaxima with dupliates
localMaxima' :: [Integer] -> [Integer]
localMaxima' list
    | length list <= 2 = []
    | (maximum (take 3 list)) == (take 3 list) !! 1 = (take 3 list) !! 1 : localMaxima (drop 2 list)
    | otherwise = localMaxima (drop 1 list)


localMaxima :: [Integer] -> [Integer]
localMaxima = filterDup . localMaxima'

filterDup :: [Integer] -> [Integer]
filterDup [] = []
filterDup (x:[]) = [x]
filterDup (x:y:rest) 
  | x == y = (filterDup (y:rest))
  | otherwise = x:(filterDup (y:rest))



-- histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers). Your output must exactly match the output shown in the
-- examples below.
-- histogram [1,1,1,5] ==
-- *
-- *
-- *   *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
-- *
-- *
-- * *
-- ******  *
-- ==========
-- 0123456789


--histogram ints = unlines $ (map asterisks ints) ++ ["==========", "0123456789"]
--
data Occurences = Occ [Integer] Occurences | Empty
  deriving Show

createHash :: Integer -> Occurences -> Occurences
createHash i Empty = Occ [i] Empty
createHash i (Occ values occ)
  | i `elem` values = Occ values (createHash i occ)
  | otherwise = Occ (i:values) occ

asterisks :: Occurences -> [String]
asterisks Empty = []
asterisks (Occ values occ) = draw (sort values) 0 : asterisks occ

draw :: [Integer] -> Int -> String
draw _ 10 = ""
draw [] _ = ""
draw (x:xs) pos
  | (fromIntegral x) == pos = "*" ++ (draw xs (pos + 1))
  | otherwise = " " ++ (draw (x:xs) (pos + 1))

histogram :: [Integer] -> String
histogram = unlines . reverse . drawText

drawText :: [Integer] -> [String]
drawText list = ["0123456789", "=========="] ++ (asterisks $ createOccurences list)

createOccurences :: [Integer] -> Occurences
createOccurences list = foldr createHash Empty list
  
