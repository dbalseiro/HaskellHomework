type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
performMove :: Integer -> Peg -> Peg -> Peg -> [Move]
concatenateMoves :: [Move] -> [Move] -> [Move]
display :: [Move] -> IO()

concatenateMoves [] y = y
concatenateMoves (x:xs) y = x : (concatenateMoves xs y)

performMove 0 _ _ _ = []
performMove n from to using = concatenateMoves
    (concatenateMoves (performMove (n-1) from using to) [(from, to)])
    (performMove (n-1) using to from)

hanoi n a b c = performMove n a c b

display [] = return()
display (x:xs) = do {
    putStrLn (fst x ++ "," ++ snd x);
    display xs
}

main = display (hanoi 2 "a" "b" "c")


