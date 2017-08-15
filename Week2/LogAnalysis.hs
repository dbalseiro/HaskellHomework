{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log
import Text.Read

data ValidMessageType = ValidMessageType MessageType TimeStamp [String]
                      | InvalidMessageType
                      deriving Eq

-- Try to get the message type.. if it is not valid, then the log is unknown
parseMessage :: String -> LogMessage
parseMessage message = 
    case parseMessageType (words message) of
      InvalidMessageType -> Unknown message
      ValidMessageType messageType timestamp rest -> 
        LogMessage messageType timestamp (unwords rest)

-- for each message type, i try to get the timestamp
-- for error messages, try to get the rror code first
parseMessageType :: [String] -> ValidMessageType
parseMessageType [] = InvalidMessageType
parseMessageType ("I":rest) = getTimeStamp Info rest
parseMessageType ("W":rest) = getTimeStamp Warning rest
parseMessageType ("E":rest) = getError rest
parseMessageType _ = InvalidMessageType

-- try to get first the error code and then the timestamo
-- if the error code is not valid returns Invalid
getError :: [String] -> ValidMessageType
getError [] = InvalidMessageType
getError (errorCode:rest) = 
  case validInt errorCode of
    InvalidInt -> InvalidMessageType
    (ValidInt i) -> getTimeStamp (Error i) rest

--try to get timestamp if valid
getTimeStamp :: MessageType -> [String] -> ValidMessageType
getTimeStamp _ [] = InvalidMessageType
getTimeStamp messageType (timestamp:rest) =
  case validInt timestamp of
    InvalidInt -> InvalidMessageType
    (ValidInt i) -> ValidMessageType messageType i rest

data ValidInt = ValidInt Int
              | InvalidInt
              deriving (Show, Eq)

validInt :: String -> ValidInt
validInt str =
  case readMaybe str of
    Nothing -> InvalidInt
    (Just i) -> ValidInt i

parse :: String -> [LogMessage]
parse = parse' . lines

parse' :: [String] -> [LogMessage]
parse' [] = []
parse' (x:xs) = parseMessage x : parse' xs

-- inserts a new LogMessage into an existing MessageTree, pro- ducing a 
-- new MessageTree. insert may assume that it is given a sorted MessageTree, 
-- and must produce a new sorted MessageTree containing the new LogMessage in 
-- addition to the contents of the original MessageTree.
-- However, note that if insert is given a LogMessage which is Unknown, it 
-- should return the MessageTree unchanged.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage _ _ _) tree@(Node _ (Unknown _) _) = tree
insert logMessage@(LogMessage _ timestamp _) (Node left content@(LogMessage _ treeTimestamp _) right) =
    if timestamp < treeTimestamp
        then Node (insert logMessage left) content right
        else Node left content (insert logMessage right)

--which builds up a MessageTree containing the messages in the list, by 
--successively inserting the messages into a MessageTree (beginning with a Leaf).
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

--build [(parseMessage "E 2 147 1"), (parseMessage "E 2 148 2"), (parseMessage "E 2 146 3")]

