{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Session.App where

import           Account.Model
import           Control.Monad.Except                (MonadError)
import           Control.Monad.Free                  ((>=>))
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Reader                (MonadReader)
import qualified Control.Monad.Reader                as Reader
import           Control.Monad.Trans                 (liftIO)
import           Control.Monad.Trans.Maybe           as Maybe
import qualified Data.Aeson                          as Aeson
import qualified Data.ByteString.Char8               as B8
import           Data.Functor                        ((<&>))
import qualified Data.HashMap.Strict                 as HM
import           Data.Maybe                          (listToMaybe)
import qualified Data.Maybe                          as Maybe
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeUtf8)
import           Data.UUID                           (UUID)
import qualified Data.UUID                           as UUID
import           Database.PostgreSQL.Simple          (Connection, query)
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import qualified Network.HTTP.Simple                 as HTTP

import           Github.App
import           Model
import           PatchGirl
import           Servant
import           Servant.API.ResponseHeaders         (noHeader)
import           Servant.Auth.Server                 (AuthResult (..),
                                                      CookieSettings,
                                                      JWTSettings, SetCookie,
                                                      acceptLogin, clearSession,
                                                      makeXsrfCookie)
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Server                      (ServerError)
import           Session.Model
import           Web.Cookie                          (setCookieValue)


visitorId :: UUID
visitorId =
  Maybe.fromJust $ UUID.fromString "00000000-0000-1000-a000-000000000000"


-- * who am I


whoAmIHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> AuthResult CookieSession
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie
                 ]
         Session)
whoAmIHandler cookieSettings jwtSettings = \case
  Authenticated SignedUserCookie {..} -> do
    csrfToken <- liftIO $ createCsrfToken cookieSettings
    let (CaseInsensitive email) = _cookieAccountEmail
    return $
      noHeader $ noHeader $ SignedUserSession { _sessionAccountId = _cookieAccountId
                                              , _sessionEmail = email
                                              , _sessionCsrfToken = csrfToken
                                              }

  _ ->
    createVisitorSession cookieSettings jwtSettings


-- * sign in


signInHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> SignIn
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie]
         Session)
signInHandler cookieSettings jwtSettings login = do
  connection <- getDBConnection
  mAccount <- liftIO $ selectAccount login connection
  case mAccount of
    Nothing ->
      throwError err401

    Just Account {..} -> do
      let CaseInsensitive email = _accountEmail
      let cookieSession =
            SignedUserCookie { _cookieAccountId = _accountId
                             , _cookieAccountEmail = _accountEmail
                             }
      let session =
            SignedUserSession { _sessionAccountId = _accountId
                              , _sessionEmail = email
                              , _sessionCsrfToken = ""
                              }
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings cookieSession
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies session

selectAccount :: SignIn -> Connection -> IO (Maybe Account)
selectAccount SignIn {..} connection =
  query connection selectAccountQuery (_signInEmail, _signInPassword) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          AND password = crypt(?, password);
          |]

-- * sign in on github


signInOnGithubHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> SignInWithGithub
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie]
         Session)
signInOnGithubHandler cookieSettings jwtSettings SignInWithGithub{..} = do
  GithubConfig {..} <- Reader.ask <&> githubConfig
  let
    getGithubProfile =
      Maybe.MaybeT . getGithubAccessTokenClient >=> Maybe.MaybeT . getGithubProfileClient
  let
    githubOAuthCredentials =
      GithubOAuthCredentials { _githubOAuthCredentialsClientId = T.unpack githubConfigClientId
                             , _githubOAuthCredentialsClientSecret = T.unpack githubConfigClientSecret
                             , _githubOAuthCredentialsCode = _signInWithGithubCode
                             }

  liftIO $ putStrLn $ show githubOAuthCredentials
  (liftIO . runMaybeT . getGithubProfile) githubOAuthCredentials >>= \case
    Nothing -> do
      liftIO $ putStrLn "noothing"
      createVisitorSession cookieSettings jwtSettings

    Just profile -> do
      liftIO $ putStrLn $ "profile" <> show profile
      createVisitorSession cookieSettings jwtSettings


-- * sign out


deleteSessionHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie
                 ]
         Session)
deleteSessionHandler cookieSettings =
  return $
    clearSession cookieSettings $ VisitorSession { _sessionAccountId = visitorId
                                                 , _sessionCsrfToken = ""
                                                 }

-- * util


-- ** session


createVisitorSession
  :: ( MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie
                 ]
         Session
       )
createVisitorSession cookieSettings jwtSettings = do
  let cookieSession = VisitorCookie { _cookieAccountId = visitorId }
  csrfToken <- liftIO $ createCsrfToken cookieSettings
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings cookieSession
  case mApplyCookies of
    Nothing ->
      throwError err401

    Just applyCookies ->
      return . applyCookies $ VisitorSession { _sessionAccountId = visitorId
                                             , _sessionCsrfToken = csrfToken
                                             }

createCsrfToken :: CookieSettings -> IO Text
createCsrfToken cookieSettings = do
      setCookie <- liftIO $ makeXsrfCookie cookieSettings
      return $ (decodeUtf8 . setCookieValue) setCookie
