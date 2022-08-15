{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
  deriving (Show, Eq)

getTimeStamp :: LogMessage -> Int
getTimeStamp (Unknown _) = -1
getTimeStamp (LogMessage _ timeStamp _) = timeStamp

getMessage :: LogMessage -> String
getMessage (Unknown msg) = msg
getMessage (LogMessage _ _ msg) = msg

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error severity) _ _) = severity
getSeverity _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mTree = mTree
insert newLogMessage (Node lTree treeLogMessage rTree)
  | getTimeStamp newLogMessage <= getTimeStamp treeLogMessage = Node (insert newLogMessage lTree) treeLogMessage rTree
  | otherwise = Node lTree treeLogMessage (insert newLogMessage rTree)
insert newLogMessage Leaf = Node Leaf newLogMessage Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = inOrder lTree ++ [msg] ++ inOrder rTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter (\x -> getSeverity x >= 50) . inOrder . build