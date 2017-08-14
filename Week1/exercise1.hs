toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
reverseList :: [Integer] -> [Integer]
concatList :: [Integer] -> [Integer] -> [Integer]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherRev :: [Integer] -> [Integer]

sumDigits :: [Integer] -> Integer
sumIndividualDigits :: [Integer] -> Integer

validate :: Integer -> Bool

toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits n = reverseList (toDigitsRev n)

reverseList [] = []
reverseList (x:xs) = concatList (reverseList xs) (x : [])

concatList [] y = y
concatList (x:xs) y = x : (concatList xs y)

doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = x : y * 2 : doubleEveryOtherRev(xs)

doubleEveryOther x = reverseList (doubleEveryOtherRev (reverseList x))

sumDigits [] = 0
sumDigits (x:xs) = (sumIndividualDigits (toDigits x)) + (sumDigits xs)

sumIndividualDigits [] = 0
sumIndividualDigits (x:xs) = x + (sumIndividualDigits xs)

validate i = sumDigits (doubleEveryOther (toDigits i)) `mod` 10 == 0

display i = 
    if validate i
    then putStrLn "OK"
    else putStrLn "ERR"


main = do {
    display 4012888888881881; display 4012888888881882
}

