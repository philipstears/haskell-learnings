{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage line = parseMessageCore $ words line
    where
        parseMessageCore ("E":severity:timestamp:rest) = constructMessage (Error $ read severity) timestamp rest
        parseMessageCore ("I":timestamp:rest) = constructMessage Info timestamp rest
        parseMessageCore ("W":timestamp:rest) = constructMessage Warning timestamp rest
        parseMessageCore messageWords = Unknown $ unwords messageWords 
        constructMessage messageType timestampString messageWords = LogMessage messageType (read timestampString) (unwords messageWords)

parse :: String -> [LogMessage]

parse = map parseMessage . lines 

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree

insert message Leaf = Node Leaf message Leaf 

insert message (Node left value right) 
    | message `isSameOrNewer` value = Node left value (insert message right)
    | otherwise = Node (insert message left) value right
    where
        isSameOrNewer (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 >= ts2 
        isSameOrNewer _ _ = True

build :: [LogMessage] -> MessageTree

build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []

inOrder (Node left value right) = (inOrder left) ++ [value] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong = map getMessage . inOrder . build . filter isImportant 
    where
        getMessage (LogMessage _ _ m) = m
        getMessage (Unknown m) = m
        isImportant (LogMessage (Error severity) _ _) = severity > 50
        isImportant _= False 
