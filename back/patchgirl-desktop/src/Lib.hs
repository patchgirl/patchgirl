module Lib (app) where

import qualified Control.Concurrent.Async as Async
import qualified Graphics.UI.Webviewhs    as WHS
import qualified PatchGirl.Server         as Server

app :: IO ()
app = do
  Async.race_ createWindow Server.run

createWindow :: IO ()
createWindow =
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "PatchGirl"
      , WHS.windowParamsUri        = "localhost:33726"
      , WHS.windowParamsWidth      = 1400
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }