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

-- 4 
myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == (sum . init . factors $ x)] -- () because of == ?

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

unitTests :: IO ()
unitTests = do
  assert (sumSqrs 3 == 14) "sumSqrs 3 passed" "sumSqrs 3 failed"
  assert (sumSqrs 4 == 30) "sumSqrs 4 passed" "sumSqrs 4 failed"
  assert (sumSqrs 100 == 338350) "sumSqrs 100 passed" "sumSqrs 100 failed"
  assert (grid 1 2 == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)])
    "grid 1 2 passed" "grid 1 2 failed"
  assert (square 2 == [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)])
    "square 2 passed" "square 2 failed"
  assert (myReplicate 4 True == [True, True, True, True])
    "myReplicate 4 True passed" "myReplicate 4 True failed"
  assert (myReplicate 5 1 == replicate 5 1)
    "myReplicate eq replicate passed" "myReplicate eq replicate failed"
  assert (pyths 10 == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)])
    "pyths 10 passed" "pyths 10 failed"
  assert (perfects 500 == [6, 28, 496])
    "perfects 500 passed" "perfects 500 failed"
  assert (scalarproduct [1,2,3] [4,5,6] == 32)
    "scalarproduct 32 passed" "scalarproduct 32 failed"