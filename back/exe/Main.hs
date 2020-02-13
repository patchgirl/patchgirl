
module Main where

import           App
import           System.Environment (getArgs)

main :: IO ()
main = do
  workDir : [_] <- getArgs
  run workDir
