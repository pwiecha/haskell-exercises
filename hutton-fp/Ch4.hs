module Ch4 where


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
