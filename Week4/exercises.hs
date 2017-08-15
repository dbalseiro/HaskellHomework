
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

{--
foldTree :: [a] -> Tree a
foldTree list = foldr () Leaf list
--}

addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 0 Leaf x Leaf

-- EN CASA --


xor :: [Bool] -> Bool
xor booleans = foldr (\x b -> if b then not x else x) False booleans


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


filterProtoPrimes = filter (\x -> filterProtoPrimes' x 100) (cartProd [1..10] [1..10])
filterProtoPrimes' :: (Int, Int) -> Int -> Bool
filterProtoPrimes' element limit = 
  (magicNumber (fst element) (snd element)) <= limit ||
  (fst element) <= (snd element)

magicNumber :: Int -> Int -> Int
magicNumber i j = i + j + (2 * i * j)

convertCartesianToNumber :: [(Int, Int)] -> [Int]
convertCartesianToNumber = 
