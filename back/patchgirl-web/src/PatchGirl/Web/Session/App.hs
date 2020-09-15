{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module PatchGirl.Web.Session.App where

import           Control.Monad.Except                (MonadError)
import           Control.Monad.Free                  ((>=>))
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Reader                (MonadReader)
import qualified Control.Monad.Reader                as Reader
import           Control.Monad.Trans                 (liftIO)
import           Control.Monad.Trans.Maybe           as Maybe
import           Data.Functor                        ((<&>))
import qualified Data.Maybe                          as Maybe
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeUtf8)
import           Data.UUID                           (UUID)
import qualified Data.UUID                           as UUID
import           Web.Cookie                          (setCookieValue)
import           Servant
import           Servant.Auth.Server                 (AuthResult (..),
                                                      CookieSettings,
                                                      JWTSettings, SetCookie,
                                                      acceptLogin, clearSession,
                                                      makeXsrfCookie)
import           Servant.Auth.Server.SetCookieOrphan ()

import           PatchGirl.Web.Account.Sql
import           PatchGirl.Web.DB
import           PatchGirl.Web.CaseInsensitive
import           PatchGirl.Web.Github.App
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.Session.Model


visitorId :: UUID
visitorId =
  Maybe.fromJust $ UUID.fromString "00000000-0000-1000-a000-000000000000"


-- * who am I


whoAmIHandler
  :: ( MonadReader Env m
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
    let email = case _cookieGithubEmail of
          Just (CaseInsensitive email) -> Just email
          Nothing                      -> Nothing
    return . noHeader . noHeader $
      SignedUserSession { _sessionAccountId = _cookieAccountId
                        , _sessionGithubEmail = email
                        , _sessionCsrfToken = csrfToken
                        , _sessionGithubAvatarUrl = _cookieGithubAvatarUrl
                        }

  _ ->
    createVisitorSession cookieSettings jwtSettings


-- * sign in on github


signInOnGithubHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> SignInWithGithub
  -> m ( Headers '[ Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] Session
       )
signInOnGithubHandler cookieSettings jwtSettings SignInWithGithub{..} = do
  githubConfig <- Reader.ask <&> _envGithub
  (liftIO . runMaybeT . getGithubProfile) (mkGithubOAuthCredentials githubConfig) >>= \case
    Nothing -> do
      createVisitorSession cookieSettings jwtSettings

    Just githubProfile@GithubProfile {..} -> do
      connection <- getDBConnection
      mAccountId <- liftIO $ selectAccountFromGithubId _githubProfileId connection
      case mAccountId of
        Just accountId -> do
          createSignedUserSession cookieSettings jwtSettings githubProfile accountId

        Nothing -> do
          accountId <- liftIO $ insertAccount _githubProfileId connection
          createSignedUserSession cookieSettings jwtSettings githubProfile accountId
  where
    getGithubProfile :: GithubOAuthCredentials -> MaybeT IO GithubProfile
    getGithubProfile =
      Maybe.MaybeT . getGithubAccessTokenClient >=> Maybe.MaybeT . getGithubProfileClient

    mkGithubOAuthCredentials :: GithubConfig -> GithubOAuthCredentials
    mkGithubOAuthCredentials GithubConfig {..} =
      GithubOAuthCredentials { _githubOAuthCredentialsClientId = T.unpack _githubConfigClientId
                             , _githubOAuthCredentialsClientSecret = T.unpack _githubConfigClientSecret
                             , _githubOAuthCredentialsCode = _signInWithGithubCode
                             }

-- * sign out


deleteSessionHandler
  :: ( MonadReader Env m
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


-- *** visitor session


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


-- *** signed user session


createSignedUserSession
  :: ( MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> GithubProfile
  -> UUID
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie
                 ]
         Session
       )
createSignedUserSession cookieSettings jwtSettings GithubProfile {..} accountId = do
  let cookieSession =
        SignedUserCookie { _cookieAccountId = accountId
                         , _cookieGithubEmail = _githubProfileEmail <&> CaseInsensitive
                         , _cookieGithubAvatarUrl = _githubProfileAvatarUrl
                         }
  csrfToken <- liftIO $ createCsrfToken cookieSettings
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings cookieSession
  case mApplyCookies of
    Nothing ->
      throwError err401

    Just applyCookies ->
      return . applyCookies $ SignedUserSession { _sessionAccountId = accountId
                                                , _sessionCsrfToken = csrfToken
                                                , _sessionGithubEmail = _githubProfileEmail
                                                , _sessionGithubAvatarUrl = _githubProfileAvatarUrl
                                                }

createCsrfToken :: CookieSettings -> IO Text
createCsrfToken cookieSettings = do
      setCookie <- liftIO $ makeXsrfCookie cookieSettings
      return $ (decodeUtf8 . setCookieValue) setCookie
