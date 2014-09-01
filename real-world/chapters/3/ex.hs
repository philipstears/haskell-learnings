import Data.List

stLength [] = 0

stLength (x:xs) = 1 + (stLength xs)


stLength2 :: [a] -> Int

stLength2 [] = 0

stLength2 (_:remaining) = calculateLengthInner 1 remaining
    where
        calculateLengthInner acc [] = acc
        calculateLengthInner acc (_:xs) = calculateLengthInner (acc + 1) xs

stMean xs = (sum xs) / (fromIntegral (length (xs)))

stMean2 xs = calculateMeanInner 0 0 xs
    where
        calculateMeanInner sum count [] = sum / (fromIntegral count)
        calculateMeanInner sum count (x:xs) = calculateMeanInner (sum + x) (count + 1) xs

stPal xs = xs ++ (reverse xs)

stIsPal [] = True

stIsPal (x:[]) = True

stIsPal xs = 
    if headValue == lastValue then
        stIsPal core
    else
        False
    where
        headValue = head xs
        lastValue = last xs
        core = (tail (init xs)) 

stSortListsByLength :: [[a]] -> [[a]]

stSortListsByLength xs = sortBy compareLists xs
    where compareLists list1 list2 = compare (length list1) (length list2)

stIntersperse :: String -> [String] -> String

stIntersperse _ [] = ""

stIntersperse _ (x:[]) = x

stIntersperse delim (one:two:remainder) = one ++ delim ++ (stIntersperse delim (two:remainder))
