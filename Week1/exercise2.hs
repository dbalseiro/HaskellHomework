type Peg = String
type Move = (Peg, Peg)

main :: IO()
main = display (hanoi 2 "a" "b" "c")

display :: [Move] -> IO()
display [] = return()
display (x:xs) = do
    putStrLn (fst x ++ "," ++ snd x);
    display xs

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = performMove n a c b

performMove :: Integer -> Peg -> Peg -> Peg -> [Move]
performMove 0 _ _ _ = []
performMove n from to using = 
    (performMove (n-1) from using to) ++
    [(from, to)] ++
    (performMove (n-1) using to from)

concatenateMoves :: [Move] -> [Move] -> [Move]
concatenateMoves [] y = y
concatenateMoves (x:xs) y = x : (concatenateMoves xs y)
