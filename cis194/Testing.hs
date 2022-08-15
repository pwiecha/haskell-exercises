-- ghci Testing.hs -e unitTests
module Testing where

import LogAnalysis
import Log

asrt :: Bool -> IO ()
asrt test =
    if test then putStrLn "PASSED"
    else putStrLn "FAILED"

unitTests :: IO ()
unitTests = do
    putStrLn "Testing parseMessage"
    asrt $ parseMessage "Not the right format" == Unknown "Not the right format"
    asrt $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help" 
    asrt $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la" 
    putStrLn "Testing getTimeStamp"
    asrt $ getTimeStamp (Unknown "sthsth") == -1
    asrt $ getTimeStamp (LogMessage (Error 99) 14 ("Severe error occured")) == 14
    asrt $ getTimeStamp (LogMessage Warning 5 ("Severe error occured")) == 5
    asrt $ getTimeStamp (LogMessage Info 55 ("Severe error occured")) == 55
