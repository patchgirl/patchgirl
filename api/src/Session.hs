{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes       #-}
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
import Servant.Auth.Server (SetCookie, CookieSettings, JWTSettings)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple (Connection, query, FromRow)
import Servant.Auth.Server.SetCookieOrphan ()
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Data.CaseInsensitive
import Data.Text (Text, unpack)
-- * model

data Session =
  Session { accountId :: Int
          } deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session


-- * model


-- ** account


data Account =
  Account { accountId :: Int
          , accountEmail :: CaseInsensitive
          }
  deriving (Eq, Show, Generic, FromRow)

instance FromField CaseInsensitive where
  fromField field mdata = do
    foo <- fromField field mdata :: Conversion (CI Text)
    return $ CaseInsensitive $ (unpack . original) foo


-- ** login


data Login
  = Login { email :: CaseInsensitive
          , password :: String
          }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

data CaseInsensitive
  = CaseInsensitive String
  deriving (Eq, Show, Read, Generic)

instance ToJSON CaseInsensitive
instance FromJSON CaseInsensitive

instance ToField CaseInsensitive where
    toField (CaseInsensitive s) = toField s


-- * handler

-- checkValidLogin

createSessionHandler ::
  CookieSettings
  -> JWTSettings
  -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
              NoContent)
createSessionHandler cookieSettings jwtSettings login = do
  mAccount <- liftIO (getDBConnection >>= selectAccount login)
  case mAccount of
    Just Account { accountId } -> do
      let session = Session accountId
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings session
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

    _ ->
      throwError err401

selectAccount :: Login -> Connection -> IO (Maybe Account)
selectAccount (Login { email, password }) connection = do
  (query connection selectAccountQuery $ (email, password)) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          AND password = crypt(?, password);
          |]
