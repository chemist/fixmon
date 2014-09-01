module Main where

import qualified Server 
import qualified Agent
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case args of
         ["server"] -> Server.main
         ["agent"] -> Agent.main
