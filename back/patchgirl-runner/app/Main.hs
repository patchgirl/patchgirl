module Main where

import qualified Lib
import qualified Web.Browser as Browser

main :: IO ()
main = do
  _ <- Browser.openBrowser "https://patchgirl.io"
  Lib.app
