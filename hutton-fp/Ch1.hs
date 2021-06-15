module Ch1 where

-- super concise quicksort implementation
quicksort [] = []
quicksort (x : xs) =
    quicksort smaller ++ [x] ++ quicksort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]