import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

loop acc [] = acc

loop acc (c:cs) = let acc' = (acc * 10) + digitToInt c in
                  loop acc' cs




asInt2 :: String -> Int

asInt2 "" = error "Must provide digits"

asInt2 s = foldl addDigit 0 s
    where
        addDigit acc c = acc * 10 + digitToInt c


asInt3 :: String -> Maybe Int

asInt3 "" = Nothing

asInt3 s = Just $ foldl (\acc value -> acc * 10 + digitToInt value) 0 s
