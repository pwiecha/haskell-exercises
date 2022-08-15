{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- 1
rInt :: String -> Int
rInt = read

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("I":ts:msg') -> LogMessage Info (rInt ts) (unwords msg')
    ("W":ts:msg') -> LogMessage Warning (rInt ts) (unwords msg')
    ("E":es:ts:msg') -> LogMessage (Error $ rInt es) (rInt ts) (unwords msg')
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

data MessageTree = 
  Leaf
  | Node MessageTree LogMessage MessageTree
  deriving Show

getTimeStamp :: LogMessage -> Int
getTimeStamp (Unknown _) = -1
getTimeStamp (LogMessage _ timeStamp _) = timeStamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mTree = mTree
insert newLogMessage (Node lTree treeLogMessage rTree)
  | getTimeStamp newLogMessage <= getTimeStamp treeLogMessage = Node (insert newLogMessage lTree) treeLogMessage rTree
  | otherwise = Node lTree treeLogMessage (insert newLogMessage rTree)
insert newLogMessage Leaf = Node Leaf newLogMessage Leaf
