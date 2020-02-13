
module Main where

import           App
import           System.Environment (getArgs)

main :: IO ()
main = do
  [configFilePath] <- getArgs
  run configFilePath
