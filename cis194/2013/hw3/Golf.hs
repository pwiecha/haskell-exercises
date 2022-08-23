module Golf where

skips :: [a] -> [[a]]
skips l = [takeNth n l | n <- [1..length l]]

takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth 1 l = l
takeNth n l
  | n <= length l = (head . drop (n-1) $ l) : takeNth n (drop n l)
  | otherwise = []

localMaxima :: [Integer] -> [Integer]
localMaxima (x0:x1:x2:xs)
  | x1 > x0 && x1 > x2 = x1:localMaxima xs
  | otherwise = localMaxima (x1:x2:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram l = rotatedDraw [countOccurences l n | n <- [0..9]]

countOccurences :: [Integer] -> Integer -> Integer
countOccurences l n = foldr (\a b -> (if a == n then 1 else 0)+b) 0 l

drawLine :: Integer -> String
drawLine c = concat (take (fromIntegral c) (repeat " *"))

rotatedDraw :: [Integer] -> String
rotatedDraw = unlines . zipWith (\x y -> show x ++ "|" ++ y) [0..9] . map drawLine
