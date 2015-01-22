-- Austin Zheng
-- Problem Set 2

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List

-- NOTE: This file doesn't do anything without the helper files it references.

-- Exercise 1
-- Given a string, parse it into a LogMessage
-- Note that this is a crappy function; it breaks if any of the 'ts' or 'severity' strings aren't actually numbers
parseMessage :: String -> LogMessage
parseMessage x = case (words x) of
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
    ("E":severity:ts:msg) -> LogMessage (Error (read severity)) (read ts) (unwords msg)
    _ -> Unknown x

-- Note: words splits a string into individual words by whitespace; unwords joins a set of strings together separated by spaces
-- Note: read turns a string into some other type

-- Given a string representing the contents of a log file, return a list of LogMessages parsed from the input
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)


-- Exercise 2
-- Given a LogMessage and a MessageTree, return a new MessageTree resulting from the insertion of the log message into the input tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) intree = intree
insert msg@(LogMessage _ ts _) intree = case intree of
    Leaf -> (Node Leaf msg Leaf)
    (Node left this@(LogMessage _ ts2 _) right) -> 
        if ts < ts2 
            then (Node (LogAnalysis.insert msg left) this right) 
            else (Node left this (LogAnalysis.insert msg right))
    (Node _ (Unknown _) _) -> error "Unknown messages should never be in a message tree..."


-- Exercise 3
-- Given a list of LogMessages, construct and return a corresponding MessageTree
build :: [LogMessage] -> MessageTree
build x = buildHelper x Leaf

buildHelper :: [LogMessage] -> MessageTree -> MessageTree
buildHelper [] tree = tree
buildHelper (x:xs) tree = buildHelper xs (LogAnalysis.insert x tree)


-- Exercise 4
-- Given a MessageTree, return a list of LogMessages resulting from an in-order traversal of the tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left this right) = (inOrder left) ++ [this] ++ (inOrder right)


-- Exercise 5
-- Given an unsorted list of LogMessages, return a chronologically ordered list of raw messages that meet our importance criteria
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong unsorted = map msgFromItem (filter msgIsImportant (inOrder (build unsorted)))

-- Given a LogMessage, return the raw message
msgFromItem :: LogMessage -> String
msgFromItem (Unknown msg) = msg
msgFromItem (LogMessage _ _ msg) = msg 

-- Given a LogMessage, return whether or not we should consider it important (e.g. it's an error whose severity is at least 50)
msgIsImportant :: LogMessage -> Bool
msgIsImportant (Unknown _) = False
msgIsImportant (LogMessage t _ _) = case t of
    (Error sev) -> sev >= 50
    _ -> False

-- Test function. Get more messages
mustard :: LogMessage -> Bool
mustard (Unknown _) = False
mustard (LogMessage t _ msg) = case t of
    (Error sev) -> sev >= 50 || (isInfixOf "mustard" msg)
    Warning -> (isInfixOf "mustard" msg)
    Info -> (isInfixOf "mustard" msg)
