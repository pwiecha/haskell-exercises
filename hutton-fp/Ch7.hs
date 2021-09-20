module Ch7 where
import MyUnitTest

-- definition with implicit argument, cleaner,
-- hints that composition returns a function
myListComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myListComp f g = map f . filter g

myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = True
myAll f (x:xs) = f x && myAll f xs

myAll' :: (a -> Bool) -> [a] -> Bool
myAll' f = and . map f

myAll'' :: (a -> Bool) -> [a] -> Bool
myAll'' f = foldr (\x y -> f x && y) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = or . map f

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f [] = False
myAny' f (x:xs) = f x || myAny' f xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f = foldr (\x y -> f x || y) False

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x:xs)
  | f x = x : myTakeWhile f xs
  | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []  = []
myDropWhile f (x:xs)
  | f x = myDropWhile f xs
  | otherwise = xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x:xs else xs) []

dec2int :: [Int] -> Int
{--
dec2int lst = read num :: Int
  where num = foldl (\acc str -> acc ++ show str) "" lst
--}
dec2int = foldl (\acc num -> 10 * acc + num) 0
{--
dec2int [2,3,4,5]
-> 10 * 0 + 2 = 2
-> 10 * 2 + 3 = 23
-> 10 * 23 + 4 = 234
-> 10 * 234 + 5 = 2345
--}

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \a b -> f (a,b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry cf = \(a,b) -> cf a b


unitTests :: IO ()
unitTests = do
  assert (myListComp (^2) even [1..5] == [4,16]) "myListComp t1 passed" "myListComp t1 failed"
  assert (myListComp (^2) even [1..5] == []) "myListComp t1 passed" "myListComp t1 failed"
  assert (myAll (>5) [6..10] == True) "myall true test pass" "myall true test fail"
  asrt (myAll (>5) [4..10] == False) "myall false test"
  assert (myAll (>5) [6..10] == all (>5) [6..10]) "myall 2 pass" "myall 2 fail"
  assert (myAll (>5) [6..10] == myAll' (>5) [6..10]) "myall == myall' pass" "myall == myall' fail"
  asrt (myAny (>3) [1..4] == True) "myAny true check"
  asrt (myAny (>3) [1..2] == False) "myAny false check"
  asrt (myAny (>3) [1..2] == myAny' (>3) [1..2]) "myAny' equiv check"
  asrt (myAny'' (>3) [1..2] == myAny' (>3) [1..2]) "myAny'' equiv check"
  asrt (myTakeWhile (>3) [5,4..1] == [5,4]) "myTakeWhile test"
  asrt (myTakeWhile (>3) [] == []) "myTakeWhile empty test"
  asrt (myTakeWhile (>3) [5,4,3,5,5] == [5,4]) "myTakeWhile test 2"
  asrt (myDropWhile (>3) [5,4..1] == [3,2,1]) "myDropWhile test"
  asrt (myDropWhile (>3) [] == []) "myDropWhile empty test"
  asrt (myDropWhile (>3) [5,4,3,4,5] == [4,5]) "myDropWhile test 2"
  asrt (myMap (+2) [1..3] == [3,4,5]) "myMap test"
  asrt (myFilter (>3) [1..5] == filter (>3) [1..5]) "myFilter equiv test"