-- ex1
toDigitsRev :: Integer -> [Integer]
toDigitsRev i
    | i <= 0 = []
    | otherwise = i `mod` 10 : toDigitsRev (i `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [i] = [i]
doubleEveryOther (i:i':is) = i : 2*i' : doubleEveryOther is

-- ex3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum. toDigitsRev)

-- ex4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev