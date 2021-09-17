module Ch7 where
import MyUnitTest

-- definition with implicit argument, cleaner,
-- hints that composition returns a function
myListComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myListComp f g = map f . filter g

myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = True
myAll f (x:xs) = (f x) && (myAll f xs)

myAll' :: (a -> Bool) -> [a] -> Bool
myAll' f = and . map f

unitTests :: IO ()
unitTests = do
  assert (myListComp (^2) even [1..5] == [4,16]) "myListComp t1 passed" "myListComp t1 failed"
  assert (myListComp (^2) even [1..5] == []) "myListComp t1 passed" "myListComp t1 failed"
  assert (myAll (>5) [6..10] == True) "myall true test pass" "myall true test fail"
  asrt (myAll (>5) [4..10] == False) "myall false test"
  assert (myAll (>5) [6..10] == all (>5) [6..10]) "myall 2 pass" "myall 2 fail"
  assert (myAll (>5) [6..10] == myAll' (>5) [6..10]) "myall == myall' pass" "myall == myall' fail"