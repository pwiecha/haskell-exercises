-- ghci Testing.hs -e unitTests
module TestingBase where

asrt :: Bool -> IO ()
asrt test =
    if test then putStrLn "PASSED"
    else putStrLn "FAILED"
