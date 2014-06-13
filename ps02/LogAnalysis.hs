{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


-- Exercise 1 --

parseMessage :: String -> LogMessage
parseMessage line = 
  case (words line) of
       "I":timestamp:rst -> LogMessage Info (read timestamp) (unwords rst)
       "W":timestamp:rst -> LogMessage Warning (read timestamp) (unwords rst)
       "E":level:timestamp:rst -> LogMessage (Error (read level)) (read timestamp) (unwords rst)
       _ -> Unknown line
 

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Exercise 2 --

insert :: LogMessage -> MessageTree -> MessageTree
insert newMessage Leaf = Node Leaf newMessage Leaf
insert newMessage@(LogMessage _ t _) (Node left lm@(LogMessage _ s _) right)
    | t > s = Node left lm (insert newMessage right)
    | otherwise = Node (insert newMessage left) lm right       
insert _ m = m

-- Exercise 3 --
build :: [LogMessage] -> MessageTree
build lms = foldr insert (Leaf) lms

-- Exercise 4 --
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node (Leaf) lm right) = [lm] ++ inOrder right
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right
                           


-- Exercise 5 --

severe :: LogMessage -> Bool
severe (LogMessage (Error x) _ _)
    | x >= 50 = True
    | otherwise = False
severe _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getMessage $ inOrder $ build $ filter severe lms
                
