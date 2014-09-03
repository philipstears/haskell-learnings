{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage line = parseMessageCore $ words line
    where
        parseMessageCore ("E":severity:timestamp:rest) = LogMessage (Error (read severity :: Int)) (read timestamp :: Int) (unwords rest)
