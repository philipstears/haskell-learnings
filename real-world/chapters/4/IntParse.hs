import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

loop acc [] = acc

loop acc (c:cs) = let acc' = (acc * 10) + digitToInt c in
                  loop acc' cs
