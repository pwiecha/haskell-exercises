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
getThirdV2

unitTests :: IO ()
unitTests = do
  -- 1
  assert (halve [1..6] == ([1,2,3], [4,5,6])) "halve ut1 pass" "halve ut1 fail"
  assert (halve [1..6] == halveV2 [1..6]) "halve == halveV2 ut1 passed" "halve == halveV2 ut1 fail"
