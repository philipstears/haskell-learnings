toDigitsRev :: Integer -> [Integer]

toDigitsRev n
    | n <= 0    = []
    | otherwise = n `rem` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = x : y * 2 : doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherRev $ reverse xs

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit n = (n `rem` 10) + sumDigit (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map sumDigit xs

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `rem` 10 == 0
