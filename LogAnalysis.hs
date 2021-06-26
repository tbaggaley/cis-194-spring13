{-# OPTIONS_GHC  -Wall #-}

module LogAnaylsis where

import           Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("E" : severity : timestamp : message) ->
    LogMessage (Error (read severity)) (read timestamp) (unwords message)
  ("I" : timestamp : message) ->
    LogMessage Info (read timestamp) (unwords message)
  ("W" : timestamp : message) ->
    LogMessage Warning (read timestamp) (unwords message)
  _ -> Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (   Unknown _               ) msgTree = msgTree
insert lm@(LogMessage _ timestamp _) msgTree = case msgTree of
  Leaf -> Node Leaf lm Leaf
  (Node left lm'@(LogMessage _ timestamp' _) right) ->
    if timestamp <= timestamp'
      then Node (insert lm left) lm' right
      else Node left lm' (insert lm right)
  (Node _ (Unknown _) _) -> msgTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                 = []
inOrder (Node Leaf lm Leaf ) = [lm]
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg (Unknown msg       ) = msg

whatWentWrong :: [LogMessage] -> String
whatWentWrong =
  foldr (\a b -> getMsg a ++ "\n" ++ b) ""
    . filter highSeverity
    . inOrder
    . build
 where
  highSeverity (LogMessage (Error severity) _ _) = severity > 50
  highSeverity _ = False
