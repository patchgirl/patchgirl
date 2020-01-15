{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Account.App where

import           Account.Model
import           Control.Monad.Except                (MonadError)
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Reader                (MonadReader)
import           Control.Monad.Trans                 (liftIO)
import           Data.ByteString.UTF8                as BSU
import           Data.Either                         (isLeft)
import           Data.Functor                        ((<&>))
import           Data.Maybe                          (listToMaybe)
import           Data.Maybe                          (isJust)
import           Data.Text                           (Text)
import           Data.Text.Encoding                  (decodeUtf8)
import           Database.PostgreSQL.Simple          (Connection, Only (..),
                                                      execute, query)
import           Database.PostgreSQL.Simple.FromRow  (FromRow (..))
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           Mailgun.App
import           Model
import           PatchGirl
import           Servant                             (Header, Headers, err400,
                                                      err401, throwError)
import           Servant.API.ResponseHeaders         (noHeader)
import           Servant.Auth.Server
import           Servant.Auth.Server                 (CookieSettings,
                                                      JWTSettings, SetCookie)
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Server                      (ServerError)
import           Session.Model
import qualified Text.Email.Validate                 as Email
import           Web.Cookie                          (setCookieValue)

-- * initialize password


initializePasswordHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => InitializePassword
  -> m ()
initializePasswordHandler initializePassword = do
  connection <- liftIO getDBConnection
  mAccount <- liftIO $ selectAccountFromInitializePassword initializePassword connection
  case mAccount of
    Just _ -> do
      liftIO $ setPassword initializePassword connection
      return ()

    Nothing ->
      throwError err400

selectAccountFromInitializePassword :: InitializePassword -> Connection -> IO (Maybe Account)
selectAccountFromInitializePassword (InitializePassword { _initializePasswordAccountId
                                                        , _initializePasswordEmail
                                                        , _initializePasswordToken }) connection = do
  (query connection selectAccountQuery ( _initializePasswordAccountId
                                       , _initializePasswordEmail
                                       , _initializePasswordToken
                                       )) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE id = ?
          AND email = ?
          AND signup_token = ?
          |]

setPassword :: InitializePassword -> Connection -> IO ()
setPassword (InitializePassword { _initializePasswordPassword
                                , _initializePasswordAccountId }) connection = do
  _ <- execute connection selectAccountQuery ( _initializePasswordPassword
                                             , _initializePasswordAccountId
                                             )
  return ()
  where
    selectAccountQuery =
      [sql|
          UPDATE account
          SET password = crypt(?, gen_salt('bf', 8))
          WHERE id = ?
          |]