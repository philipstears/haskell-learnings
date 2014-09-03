{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage line = parseMessageCore $ words line
    where
        parseMessageCore ("E":severity:timestamp:rest) = constructMessage (Error (read severity :: Int)) timestamp rest
        parseMessageCore ("I":timestamp:rest) = constructMessage Info timestamp rest
        parseMessageCore ("W":timestamp:rest) = constructMessage Warning timestamp rest
        parseMessageCore messageWords = error $ "Invalid message line: " ++ (unwords messageWords)
        constructMessage messageType timestampString messageWords = LogMessage messageType (read timestampString :: Int) (unwords messageWords)

parse :: String -> [LogMessage]

parse logFile = map parseMessage (lines logFile)
