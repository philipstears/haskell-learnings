square :: [Double] -> [Double]

square [] = []

square (x:xs) = x ^ 2 : square xs

square2 xs = map (^2) xs

stMap _ [] = []

stMap f (x:xs) = (f x) : (stMap f xs)

stFilter :: (a -> Bool) -> [a] -> [a]

stFilter _ [] = []

stFilter f xs = helper [] f xs
    where
        helper acc _ [] = acc
        helper acc f (x:xs) | True == f x = helper (x:acc) f xs
                            | False == f x = helper acc f xs 
