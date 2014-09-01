stReverse [] = []

stReverse (x:xs) = stReverseInner [x] xs
    where
        stReverseInner acc [] = acc
        stReverseInner acc (x:xs) = stReverseInner (x:acc) xs
