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
