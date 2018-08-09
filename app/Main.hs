module Main where

import Printer
import System.Environment

main :: IO ()
main = do
  a <- getArgs
  pn <- getProgName
  if length a < 1 || length a > 3
    then putStrLn $ "usage: " ++ pn ++ "[[hh] mm] ss"
    else printTimer a
