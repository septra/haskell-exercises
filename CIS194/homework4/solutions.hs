-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x-2) . filter even


fun2 :: Integer -> Integer
fun2 = sum . 
       filter even . 
       takeWhile (>1) . 
       iterate (\x -> if even x then x `div` 2 else 3*x+1)

-- End Exercise 1


-- Exercise 2
data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = case splitAt (length xs `div` 2) xs of
                (xs, y:ys) -> Node 
                                (height $ length $ xs ++ y:ys) 
                                (foldTree xs) y  (foldTree ys) 
                ([],   []) -> Leaf
              where
                height x = floor $ logBase 2 (fromIntegral x)


-- End Exercise 2



-- Exercise 3
xor :: [Bool] -> Bool
xor = odd . foldl (\acc x -> if x then acc + 1 else acc) 0


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


-- Still need to wrap my head around this one. 
-- Refer: http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base


-- End Exercise 3


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filteredList [1..n]
    where
        filteredList = map fst . 
                       filter (null . snd) . 
                       map (\x -> (x, [(i,j) | j <- [1..x], 
                                               i <- [1..x],
                                               i + j + 2*i*j == x]))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


-- End Exercise 4
