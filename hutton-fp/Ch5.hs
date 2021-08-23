module Ch5 where
import MyUnitTest

-- 1
sumSqrs :: Int -> Int
sumSqrs n = sum [x^2 | x <- [1..n]] 

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3
{-
square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]
-}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

unitTests :: IO ()
unitTests = do
  assert (sumSqrs 3 == 14) "sumSqrs 3 passed" "sumSqrs 3 failed"
  assert (sumSqrs 4 == 30) "sumSqrs 4 passed" "sumSqrs 4 failed"
  assert (sumSqrs 100 == 338350) "sumSqrs 100 passed" "sumSqrs 100 failed"
  assert (grid 1 2 == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)])
    "grid 1 2 passed" "grid 1 2 failed"
  assert (square 2 == [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)])
    "square 2 passed" "square 2 failed"