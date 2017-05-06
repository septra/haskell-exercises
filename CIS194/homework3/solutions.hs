-- Relevant for Exercise 3
import qualified Data.Map as Map
import Data.Char

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
-- Imports already done at the top level
--import qualified Data.Map as Map
--import Data.Char

{- 
   The solution involves building a map of the elements that have been passed 
   passed in a list and their corresponding counts. 

   The number of lines above the baseline is determined by extracting the highest count
   in the previous map with the function `nLines`

   Then, the recursive function `buildLines` builds each horizontal line of the 
   histogram starting with the topmost one and recursively appending every subsequent line.

   The lines are built by folding the elements of [0..9], corresponding to the x-axis digits.
   If the fold element is present in the `filterMap` (to check whether the digit's count
   is >= the stack position of the line being constructed) then a "*" is returned otherwise a " ".
   `filterMap` keeps only those elements in the map, whose value-count is >= the line number)
-}

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
