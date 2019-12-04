{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Session where

import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Auth.Server (SetCookie, CookieSettings, JWTSettings)

-- * model

data Session =
  Session { sessionId :: String
          , accountId :: Int
          } deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session

data Login
  = Login { email :: String
          , password :: String
          }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

-- * handler

-- checkValidLogin

createSessionHandler ::
  CookieSettings
  -> JWTSettings
  -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
              NoContent)
createSessionHandler cookieSettings jwtSettings (Login "foo@mail.com" "123") = do
  let session = Session "foo@mail.com" 1
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings session
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent
createSessionHandler _ _ _ = throwError err401
