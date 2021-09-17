module MyUnitTest where

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
  then putStrLn passStatement
  else putStrLn failStatement

-- auto pass / fail printing
asrt :: Bool -> String -> IO ()
asrt test statement =
  if test
  then putStrLn (statement ++ " PASSED")
  else putStrLn (statement ++ " FAILED")