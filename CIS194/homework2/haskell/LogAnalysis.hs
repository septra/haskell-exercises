module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = parseList . words
    where 
        isValidError :: [String] -> Bool
        isValidError (x:xs)
          | x == "E"  = True
          | otherwise = False

        isValidNonError :: [String] -> Bool
        isValidNonError (t:_)
          | t == "I"  = True
          | t == "W"  = True
          | otherwise = False

        parseError :: [String] -> LogMessage
        parseError (t:level:stamp:message) = 
          LogMessage (Error $ read level) (read stamp) (unwords message)

        parseNonError :: [String] -> LogMessage
        parseNonError string@(t:stamp:message)
          | t == "I"  = LogMessage Info (read stamp) (unwords message)
          | t == "W"  = LogMessage Warning (read stamp) (unwords message)
          | otherwise = Unknown $ unwords string

        parseList :: [String] -> LogMessage
        parseList x
          | isValidError    x = parseError x
          | isValidNonError x = parseNonError x
          | otherwise         = Unknown $ unwords x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- End Exercise 1


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log@(LogMessage t stamp message) Leaf = Node Leaf log Leaf
insert log@(LogMessage t stamp message) (Node leftTree nodeLog@(LogMessage _ nodeStamp _) rightTree)
    | stamp < nodeStamp = Node (insert log leftTree) nodeLog rightTree
    | stamp > nodeStamp = Node leftTree nodeLog (insert log rightTree)
    -- (otherwise) What about the case of stamp == nodeStamp?

-- End Exercise 2

-- Exercise 3 
build :: [LogMessage] -> MessageTree
build = foldl (\tree msg -> insert msg tree) Leaf

-- End Exercise 3


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree log rightTree) = (inOrder leftTree) ++ [log] ++ (inOrder rightTree)

-- End Exercise 4

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . inOrder . build . filterSevereErrors
    where 
        isSevereError :: MessageType -> Bool
        isSevereError (Error x)
            | x >= 50    = True
            | otherwise = False
        isSevereError _ = False

        filterSevereErrors :: [LogMessage] -> [LogMessage]
        filterSevereErrors =  foldl 
                        (\filtered log@(LogMessage t s m) -> 
                         if isSevereError t 
                             then filtered ++ [log] 
                             else filtered
                        ) []

        extractMessage :: LogMessage -> String
        extractMessage (LogMessage _ _ msg) = msg

-- End Exercise 5



