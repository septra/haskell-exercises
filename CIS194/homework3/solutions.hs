
-- Exercise 1
skips :: [a] -> [[a]]
skips [] = []
skips xs = [xs] ++ filtern 2 xs

filtern :: Int -> [a] -> [[a]]
filtern n xs
    | n > length xs = []
    | otherwise = 
        [(map snd .  
          filter ((==0) .  (`mod` n) .  (fst)) .  
          zip [1..]) xs] ++ 
        filtern (n+1) xs

-- End Exercise 1

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : t@(y:z:xs))
    | y > x && y > z = y : localMaxima t
    | otherwise = localMaxima t
localMaxima _ = []

-- End Exercise 2
