module Lib (app) where

import qualified Graphics.UI.Webviewhs as WHS

app :: IO ()
app = do
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "PatchGirl"
      , WHS.windowParamsUri        = "https://patchgirl.io"
      , WHS.windowParamsWidth      = 1400
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
