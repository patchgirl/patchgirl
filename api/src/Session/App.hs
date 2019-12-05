{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Session.App where

import Control.Monad.Trans (liftIO)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server (SetCookie, CookieSettings, JWTSettings)
import           Database.PostgreSQL.Simple (Connection, query)
import Servant.Auth.Server.SetCookieOrphan ()
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Account.Model
import Session.Model


-- * sign in


createSessionHandler ::
  CookieSettings
  -> JWTSettings
  -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
              NoContent)
createSessionHandler cookieSettings jwtSettings login = do
  mAccount <- liftIO (getDBConnection >>= selectAccount login)
  case mAccount of
    Just Account { _accountId, _accountEmail } -> do
      let session =
            Session { accountId = _accountId
                    , accountEmail = _accountEmail
                    }
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
