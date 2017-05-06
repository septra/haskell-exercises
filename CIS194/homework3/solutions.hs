
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


-- Exercise 3
elemCounts :: [Int] -> Map.Map Int Int
elemCounts = Map.fromListWith (\x y -> x + y) . map (\x -> (x,1))

nLines :: Map.Map Int Int -> Int
nLines = Map.foldlWithKey (\acc k v -> if acc > v then acc else v) 0

buildLines 0 _ = map (\x -> '=') [0..9] ++ "\n" ++ 
                 map intToDigit  [0..9] ++ "\n"
buildLines n m = foldl (\acc x -> 
                            if Map.member x $ filterMap n m 
                                then acc ++ "*"
                                else acc ++ " ") "" [0..9] ++ 
                  "\n" ++ buildLines (n-1) m
    where filterMap el mp = Map.filter (\v -> v >= el) mp

histogram :: [Int] -> String
histogram lst = buildLines n m
    where
        m = elemCounts lst
        n = nLines m

-- End Exercise 3
