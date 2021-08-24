module Ch6 where
import MyUnitTest

-- warmup
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myDrop :: Integral b => b -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : myInit xs

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

-- 6
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
-- pattern match on nested lists is also possible
-- ((x:xs):xss)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

-- unsafe indexing
(!!.) :: [a] -> Int -> a
(!!.) (x:_) 0 = x
(!!.) (_:xs) n = (!!.) xs (n - 1)
(!!.) [] _ = error "empty list"

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e' (e:es)
  | e' == e = True
  | otherwise = elem' e' es
-- or element wise OR e' == e || elem' e' es

-- 7
-- assumes two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y = y : merge (x:xs) ys

-- 8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lhalf) (msort rhalf)
  where (lhalf, rhalf) = halve xs
-- msort xs = merge (msort . fst . halve $ xs) (msort . snd . halve $ xs)
-- msort [5,4,3,2,1] -> merge (*) (**)
-- (*) msort [5,4] -> merge (msort [5] -> [5]) (msort [4] -> [4]) -> merge [5] [4] -> [4,5]
-- (**) msort [3,2,1] -> merge (msort [3,2] -> merge (msort [3] -> [3]) (msort [2] -> [2])) (msort [1] -> [1])
-- merge [3] [2] -> [2,3] ; merge [2,3] [1] = 1 : merge [2,3] [] = 1:[2,3] = [1,2,3]
-- merge [4,5] [1,2,3] = 1:merge [4,5] [2,3] = 1:2:merge [4,5] [3] = 1:2:3:merge [4,5] [] = 1:2:3:[4,5] = [1..5]

halve :: [a] -> ([a], [a])
halve as = (take hlen as, drop hlen as)
  where hlen = length as `div` 2

-- 9
sum' :: Num a => [a] -> a
sum' xs = foldr (+) 0 xs -- function, accumulator, list to consume

take' :: Integral n => n -> [a] -> [a]
take' _ [] = [] -- empty list encountered
take' 0 (x:xs) = [] -- counter exausted
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

unitTests :: IO ()
unitTests = do
  assert (sumdown 3 == 6) "sumdown 3 passed" "sumdown 3 failed"
  assert (sumdown 10 == 55) "sumdown 10 passed" "sumdown 10 failed"
  assert ((-2) <^> 3 == (-8)) "<^> operator check pass" "<^> operator check failed"
  assert (euclid 6 27 == 3) "euclid 6 27 pass" "euclid 6 27 fail"
  assert (euclid 9 12 == 3) "euclid 9 12 pass" "euclid 9 12 fail"
  assert (merge [2,5,6] [1,3] == [1,2,3,5,6]) "merge passed" "merge failed"
  assert (msort [5,4..1] == [1,2,3,4,5]) "msort passed" "msort failed"
  assert (msort [6,5..1] == [1,2,3,4,5,6]) "msort passed test 2" "msort failed test 2"
  assert (sum [6,5..1] == 21) "sum' passed" "sum' failed"
  assert (null (take' 5 [])) "take' from empty passed" "take' from empty failed"
  assert (take' 5 [1,2,3] == [1,2,3]) "take' from small list passed" "take' from small list failed"
  assert (take' 5 [1..10] == [1..5]) "take' from big list passed" "take' from big list failed"
  assert (last' [1..10] == 10) "last' from list passed" "last' from list failed"

