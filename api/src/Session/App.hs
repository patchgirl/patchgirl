{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Session.App where

import           Account.Model
import           Control.Monad.Trans                 (liftIO)
import           Data.Functor                        ((<&>))
import           Data.Maybe                          (listToMaybe)
import           Data.Text                           (Text)
import           Data.Text.Encoding                  (decodeUtf8)
import           Database.PostgreSQL.Simple          (Connection, query)
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           Model
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server                 (CookieSettings,
                                                      JWTSettings, SetCookie)
import           Servant.Auth.Server.SetCookieOrphan ()
import           Session.Model
import           Web.Cookie                          (setCookieValue)


-- * who am I

whoAmIHandler
  :: CookieSettings
  -> JWTSettings
  -> AuthResult CookieSession
  -> Handler Session
whoAmIHandler cookieSettings _ = \case
  (Authenticated (SignedUserCookie { _cookieAccountId, _cookieAccountEmail })) -> do
    --csrfToken <- liftIO $ createCsrfToken
    let (CaseInsensitive email) = _cookieAccountEmail
    return $
      SignedUserSession { _sessionAccountId = _cookieAccountId
                        , _sessionEmail = email
                        , _sessionCsrfToken = "" -- csrfToken
                        }

  e -> do
    liftIO $ liftIO $ print e
    --csrfToken <- liftIO $ createCsrfToken
    return $
      VisitorSession { _sessionAccountId = 1
                     , _sessionCsrfToken = "" -- csrfToken
                     }
  where
    createCsrfToken :: IO Text
    createCsrfToken = do
      setCookie <- liftIO $ makeXsrfCookie cookieSettings
      return $ (decodeUtf8 . setCookieValue) setCookie


-- * sign in


createSessionHandler
  :: CookieSettings
  -> JWTSettings
  -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie]
              NoContent)
createSessionHandler cookieSettings jwtSettings login = do
  mAccount <- liftIO (getDBConnection >>= selectAccount login)
  case mAccount of
    Nothing ->
      throwError err401

    Just (Account { _accountId, _accountEmail }) -> do
      let cookieSession =
            SignedUserCookie { _cookieAccountId = _accountId
                             , _cookieAccountEmail = _accountEmail
                             }
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings cookieSession
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

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
