module Lib where

import qualified Graphics.UI.Webviewhs as WHS

main :: IO ()
main = do
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "Test"
      , WHS.windowParamsUri        = "https://patchgirl.io"
      , WHS.windowParamsWidth      = 1400
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
