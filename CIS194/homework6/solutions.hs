{-# LANGUAGE FlexibleInstances #-}

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


-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

mulByInt n (Cons x stream) = Cons (n*x) $ mulByInt n stream
addByInt n (Cons x stream) = Cons (n+x) stream

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamFromSeed (*x) (0*x))
    negate (Cons x stream) = Cons (-x) (negate stream)
    (+) (Cons x (stream1)) (Cons y (stream2)) = 
                                Cons (x + y) (stream1 + stream2)
    (*) (Cons x (stream1)) s2@(Cons y (stream2)) = 
                                Cons (x * y) ((mulByInt x stream2) + (stream1 * s2))

instance Fractional (Stream Integer) where
    (/) (Cons a (stream1)) (Cons b (stream2)) = q
        where q = Cons (div a b) $ mulByInt (div 1 b) (stream1 - q * stream2)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- End Exercise 6


-- Exercise 7

data Matrix4 = Matrix4 Integer Integer Integer Integer 
    deriving (Show)

instance Num Matrix4 where
    fromInteger x
        | x == 1    = Matrix4 1 1 1 0
        | otherwise = fromInteger 1 ^ x
    (*) (Matrix4 a1 b1 c1 d1) (Matrix4 a2 b2 c2 d2) = 
        Matrix4 ((a1 * a2) + (b1 * c2)) ((a1 * b2) + (b1 * d2))
                ((c1 * a2) + (d1 * c2)) ((c1 * b2) + (d1 * d2))

fib4 :: Integer -> Integer
fib4 x
    | x == 0    = 0
    | otherwise = extractN $ fromInteger x
    where extractN (Matrix4 _ _ n _) = n


-- End Exercise 7


