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
    putStrLn "Testing tree insertion"
    asrt $ insert (Unknown "sthsth") Leaf == Leaf
    asrt $ insert (LogMessage Warning 35 "foo") Leaf == Node Leaf (LogMessage Warning 35 "foo") Leaf
    asrt $ insert (LogMessage Warning 35 "foo") (Node Leaf (LogMessage Info 23 "info msg") Leaf) == Node Leaf (LogMessage Info 23 "info msg") (Node Leaf (LogMessage Warning 35 "foo") Leaf)
    putStrLn "Testing tree buildup"
    asrt $ build [Unknown "unkn foo", Unknown "unkn bar"] == Leaf
    asrt $ build [Unknown "unkn foo", LogMessage Info 1 "First Info", Unknown "unkn bar"] == Node Leaf (LogMessage Info 1 "First Info") Leaf
    asrt $ build [LogMessage Warning 5 "Warning at 5", Unknown "unkn foo", LogMessage Info 1 "First Info", Unknown "unkn bar"] == Node Leaf (LogMessage Info 1 "First Info") (Node Leaf (LogMessage Warning 5 "Warning at 5") Leaf)
    asrt $ build [LogMessage Warning 2 "Warning at 2", LogMessage Warning 5 "Warning at 5", Unknown "unkn foo", LogMessage Info 3 "First Info", Unknown "unkn bar"] == Node (Node Leaf (LogMessage Warning 2 "Warning at 2") Leaf) (LogMessage Info 3 "First Info") (Node Leaf (LogMessage Warning 5 "Warning at 5") Leaf)
    putStrLn "Testing msg tree extraction"
    asrt $ inOrder (Node (Node Leaf (LogMessage Warning 2 "Warning at 2") Leaf) (LogMessage Info 3 "First Info") (Node Leaf (LogMessage Warning 5 "Warning at 5") Leaf)) == [LogMessage Warning 2 "Warning at 2", LogMessage Info 3 "First Info", LogMessage Warning 5 "Warning at 5"]
