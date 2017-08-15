main :: IO ()
main = do
    display 4012888888881881;
    display 4012888888881882

display :: Integer -> IO ()
display i = 
    if validate i
        then putStrLn "OK"
        else putStrLn "ERR"

--validate CC number
-- 
validate :: Integer -> Bool
validate i = sumDigits (doubleEveryOther (toDigits i)) `mod` 10 == 0

-- sum all digits from a list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumIndividualDigits (toDigits x)) + (sumDigits xs)

-- reverse toDigits
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

-- sum a list of integers
sumIndividualDigits :: [Integer] -> Integer
sumIndividualDigits [] = 0
sumIndividualDigits (x:xs) = x + (sumIndividualDigits xs)

-- From units onward, add digits to an array
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

--Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = concatList (reverseList xs) (x : [])

-- concatenate two lists into one
concatList :: [a] -> [a] -> [a]
concatList [] y = y
concatList (x:xs) y = x : (concatList xs y)

-- double every odd element from a list
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = x : y * 2 : doubleEveryOtherRev(xs)

-- double every od element from a list, but begining from the end.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseList (doubleEveryOtherRev (reverseList x))



