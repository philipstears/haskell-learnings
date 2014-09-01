module SplitLines (splitLines) where

    splitLines [] = []

    splitLines cs = 
        let (prefix, suffix) = break isLineTerminator cs
        in
            prefix : case suffix of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest) ->      splitLines rest
                ('\n':rest) ->      splitLines rest
                _ ->                []

    isLineTerminator c = c == '\r' || c == '\n'
