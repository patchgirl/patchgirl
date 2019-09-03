{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE ViewPatterns      #-}

module Main where

--import           Data.Default            (def)
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (ok200, status404, unauthorized401)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy
import           Yesod.Auth.OAuth2.Google

import           Health.App               as Health
import           Lib
import           Request.App              as Request

data Api = Api
  { httpManager :: Manager
  }

instance Yesod Api

instance RenderMessage Api FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesod "Api" [parseRoutes|
/health   HealthRoute GET
/requests RequestsRoute PUT
|]

getHealthRoute :: Handler Value
getHealthRoute =
  sendStatusJSON ok200 Health.getHealth

putRequestsRoute :: Handler Value
putRequestsRoute =
  sendStatusJSON ok200 Request.putRequests

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  warp 3000 $ Api manager
