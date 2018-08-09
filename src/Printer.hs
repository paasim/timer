module Printer
    ( printTimer
    ) where

import Control.Concurrent
import Control.Monad
import System.IO
import Timer

-- prepend the string with a terminal code for clearing the line
clrLn :: String -> String
clrLn s = "\r\ESC[2K" ++ s

printAndWait :: Show a => Int -> a -> IO ()
printAndWait del str = putStr (clrLn $ show str) >> threadDelay del

printAll :: Show a => [a] -> IO ()
printAll l = hSetBuffering stdout NoBuffering
           >> forM l (printAndWait 1000000)
           >> putStrLn (clrLn "Done.\a") -- \a is the bell

printTimer :: [String] -> IO ()
printTimer l = case mkDecrTimer l of
  Just t  -> printAll t
  Nothing -> putStrLn "Invalid input."

