module Ch6 where
import MyUnitTest

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3
(<^>) :: Num a => a -> Int -> a
(<^>) _ 0 = 1
(<^>) n k = n * n <^> (k - 1)

-- 4
euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a > b = euclid (a-b) b
  | a < b = euclid a (b-a)

unitTests :: IO ()
unitTests = do
  assert (sumdown 3 == 6) "sumdown 3 passed" "sumdown 3 failed"
  assert (sumdown 10 == 55) "sumdown 10 passed" "sumdown 10 failed"
  assert ((-2) <^> 3 == (-8)) "<^> operator check pass" "<^> operator check failed"
  assert (euclid 6 27 == 3) "euclid 6 27 pass" "euclid 6 27 fail"
  assert (euclid 9 12 == 3) "euclid 9 12 pass" "euclid 9 12 fail"

