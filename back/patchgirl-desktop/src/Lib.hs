module Lib (app) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text                as Text
import qualified Graphics.UI.Webviewhs    as WHS
import qualified PatchGirl.Client         as Client
import qualified PatchGirl.Server         as Server

app :: IO ()
app = do
  let mode = Client.DesktopMode
  config <- Client.getConfig mode
  Async.race_ (createWindow config) (Server.run mode)

createWindow :: Client.Config -> IO ()
createWindow config =
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "PatchGirl"
      , WHS.windowParamsUri        = "http://localhost:" <> (Text.pack $ show (Client._configPort config)) <> "/public/index.html"
      , WHS.windowParamsWidth      = 1400
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = True
      , WHS.windowParamsDebuggable = True
      }
