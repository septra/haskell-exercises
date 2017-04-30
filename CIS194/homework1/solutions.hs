-- Exercise 1
--
toDigits  :: Integer -> [Integer]
toDigits x
    | x == 0    = []
    | x < 0     = []
    | otherwise = map read $ map (\x -> [x]) (show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- End Exercise 1


-- Exercise 2
--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . map everyOther . zip [1..] . reverse 
                where everyOther x = 
                        if mod (fst x) 2 == 0 
                        then (snd x)*2 
                        else snd x

-- End Exercise 2


-- Exercise 3
--
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits [x]    = if x < 10 then x else sumDigits (toDigits x)
sumDigits (x:xs) = (sumDigits [x]) + (sumDigits xs)

-- End Exercise 3


-- Exercise 4
--
validate :: Integer -> Bool
validate x
    | x < 0 = False
    | x == 0 = False
    | otherwise = if (sumAll x) `mod` 10 == 0 then True else False
    where sumAll = sumDigits . doubleEveryOther . toDigits 

-- End Exercide 4


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- End Exercise 5


-- Exercise 6 (Optional)
--
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = 
    hanoi4 m a c b d ++ hanoi (n - m) a b d ++ hanoi4 m c b a d
    where m = quot n 2

-- End Exercise 6

-- Extra (Generalized case of N Disks and K pegs.)
    -- Code modified from: http://stackoverflow.com/questions/3607161/towers-of-hanoi-with-k-pegs
hanoiK :: Int -> [a] -> [(a, a)]
hanoiK 0 _ = []
hanoiK 1 (a : b : rest) = [(a, b)]
hanoiK n (a:b:c:rest)
    | null rest = 
            hanoiK (n - 1) (a : c : b : rest) ++
            hanoiK 1 (a : b : rest) ++
            hanoiK (n - 1) (c : b : a : rest)
    | otherwise = 
            hanoiK (quot n 2) (a : c : b : rest) ++
            hanoiK (n - (quot n 2)) (a : b : rest) ++
            hanoiK (quot n 2) (c : b : a : rest)
