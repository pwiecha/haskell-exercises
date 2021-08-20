module Ch4 where
import MyUnitTest
-- 1
halve :: [a] -> ([a], [a])
halve lst = (take half lst, drop half lst)
  where half = length lst `div` 2
-- note: length is a function with highest precedence
-- `div` is an infix operator with lower prec
-- so length will bind to lst first, then div will be called
-- on the result and 2

-- version with splitAt
halveV2 :: [a] -> ([a], [a])
halveV2 lst = splitAt (length lst `div` 2) lst

-- 2
getThirdV1 :: [a] -> a
getThirdV1 lst = lst !! 2

getThirdV2 :: [a] -> a
getThirdV2 lst = head . tail . tail $ lst

-- non-exaustive
getThirdV3 :: [a] -> a
getThirdV3 (_:_:l:_) = l

-- 3
safetailV1 :: [a] -> [a]
safetailV1 lst =
  if null lst then lst
  else tail lst

safetailV2 :: [a] -> [a]
safetailV2 lst
  | null lst = lst
  | otherwise = tail lst

safetailV3 :: [a] -> [a]
safetailV3 [] = []
safetailV3 (l:ls) = ls

-- 4
(|.|) :: Bool -> Bool -> Bool
False |.| False = False
False |.| True = True
True |.| False = True
True |.| True = True

(|..|) :: Bool -> Bool -> Bool
False |..| False = False
_ |..| _ = True

(|...|) :: Bool -> Bool -> Bool
a |...| b
  | a == b = a
  | otherwise = True

-- supports lazy evaluation, make decision based on first argument
(|....|) :: Bool -> Bool -> Bool
False |....| b = b
True |....| _ = True

-- 7
luhnDouble :: Int -> Int
luhnDouble x
  | dx > 9 = dx - 9
  | otherwise = dx
    where dx = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

unitTests :: IO ()
unitTests = do
  -- 1
  assert (halve [1..6] == ([1,2,3], [4,5,6])) "" "halve ut1 fail"
  assert (halve [1..6] == halveV2 [1..6]) "" "halve == halveV2 ut1 fail"
  -- 2
  assert (getThirdV1 [1,2,3] == 3) "" "getThirdV1 failed"
  assert (getThirdV2 [1,2,3] == 3) "" "getThirdV2 failed"
  assert (getThirdV3 [1,2,3] == 3) "" "getThirdV3 failed"
  -- 3
  assert (null $ safetailV1 [] ) "" "safetailV1 null fail"
  assert (safetailV1 [1..10] == [2..10]) "" "safetailV1 directed fail"
  assert (null $ safetailV2 [] ) "" "safetailV2 null fail"
  assert (safetailV2 [1..10] == [2..10]) "" "safetailV2 directed fail"
  assert (null $ safetailV3 [] ) "" "safetailV3 null fail"
  assert (safetailV3 [1..10] == [2..10]) "" "safetailV3 directed fail"
  -- 4
  assert (True |.| False) "" "'|.|' operator fail"
  assert (True |..| False) "" "'|..|' operator fail"
  assert (True |...| False) "" "'|...|' operator fail"
  assert (True |....| False) "" "'|....|' operator fail"
  -- 7
  assert (luhnDouble 3 == 6) "" "luhnDouble <= 9 fail"
  assert (luhnDouble 6 == 3) "" "luhnDouble > 9 fail"
  assert (luhn 1 7 8 4 == True) "" "luhn valid card fail"
  assert (luhn 4 7 8 3 == False) "" "luhn invalid card fail"
