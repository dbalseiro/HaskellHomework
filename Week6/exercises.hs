fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs :: [Integer]
fibs = map fib [0..]

fibs2 :: [Integer]
fibs2 = map snd $ iterate (\(a,b) -> (b, a+b)) (0,1)

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream head tail) = head:(streamToList tail)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream head tail) = Stream (f head) (streamMap f tail)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream (f seed) (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

odds :: Stream Ineger
odds = streamFromSeed (+2) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream e1 s1) (Stream e2 s2) = 
    Stream e1 (Stream e2 (interleaveStreams s1 s2))


