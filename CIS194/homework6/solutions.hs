-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- End Exercise 1



-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (\x y -> x + y) fibs2 (tail fibs2)


-- End Exercise 2


-- Exercise 3 
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show xs = showMax20 20 xs
        where 
            showMax20 0 _ = "\n"
            showMax20 n (Cons x (stream)) = (show x) ++ 
                                                ", " ++ 
                                            (showMax20 (n - 1) stream)

 
streamToList :: Stream a -> [a]
streamToList (Cons x stream) = x : streamToList stream

-- End Exercise 3


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a stream) = Cons (f a) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (buildStream a)
        where buildStream x = Cons (f x) (buildStream (f x))

-- End Exercise 4


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x stream1) (Cons y stream2) = 
            Cons x (Cons y (interleaveStreams stream1 stream2))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap func $ streamFromSeed (+2) 2)
    where
        func :: Integer -> Integer
        func = foldr (\x acc -> acc + 1) 0 . takeWhile even . iterate (`quot` 2)

-- End Exercise 5


