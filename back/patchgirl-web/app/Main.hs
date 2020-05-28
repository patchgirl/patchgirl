module Main where

import qualified PatchGirl.Client as Client
import qualified PatchGirl.Server as Server


main :: IO ()
main =
  Server.run Client.WebMode
