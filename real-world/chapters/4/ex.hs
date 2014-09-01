safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit [] = Nothing
safeInit (_:[]) = Just []
safeInit xs = safeInitCore [] xs
    where
        safeInitCore acc (x:[]) = Just acc
        safeInitCore acc (x:xs) = safeInitCore (x:acc) xs
