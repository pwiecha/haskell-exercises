module MyUnitTest where

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
  then putStrLn passStatement
  else putStrLn failStatement