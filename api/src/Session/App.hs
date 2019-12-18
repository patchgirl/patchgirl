{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Session.App where


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
                                                      query)
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           Mailgun.App
import           Model
import           PatchGirl
import           Servant
import           Servant.API.ResponseHeaders         (noHeader)
import           Servant.Auth.Server
import           Servant.Auth.Server                 (CookieSettings,
                                                      JWTSettings, SetCookie)
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Server                      (ServerError)
import           Session.Model
import qualified Text.Email.Validate                 as Email
import           Web.Cookie                          (setCookieValue)

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
  (Authenticated (SignedUserCookie { _cookieAccountId, _cookieAccountEmail })) -> do
    csrfToken <- liftIO $ createCsrfToken
    let (CaseInsensitive email) = _cookieAccountEmail
    return $
      noHeader $ noHeader $ SignedUserSession { _sessionAccountId = _cookieAccountId
                                              , _sessionEmail = email
                                              , _sessionCsrfToken = csrfToken
                                              }

  _ -> do
    csrfToken <- liftIO $ createCsrfToken
    let cookieSession =
          VisitorCookie { _cookieAccountId = 1 }
    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings cookieSession
    case mApplyCookies of
      Nothing           -> throwError err401
      Just applyCookies -> return $ applyCookies $ VisitorSession { _sessionAccountId = 1
                                                                  , _sessionCsrfToken = csrfToken
                                                                  }
  where
    createCsrfToken :: IO Text
    createCsrfToken = do
      setCookie <- liftIO $ makeXsrfCookie cookieSettings
      return $ (decodeUtf8 . setCookieValue) setCookie


-- * sign in


signInHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> JWTSettings
  -> Login
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie]
         Session)
signInHandler cookieSettings jwtSettings login = do
  mAccount <- liftIO (getDBConnection >>= selectAccount login)
  case mAccount of
    Nothing ->
      throwError err401

    Just (Account { _accountId, _accountEmail }) -> do
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

selectAccount :: Login -> Connection -> IO (Maybe Account)
selectAccount (Login { _email, _password }) connection = do
  (query connection selectAccountQuery $ (_email, _password)) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          AND password = crypt(?, password);
          |]


-- * sign out

deleteSessionHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => CookieSettings
  -> m (Headers '[ Header "Set-Cookie" SetCookie
                 , Header "Set-Cookie" SetCookie]
         Session)
deleteSessionHandler cookieSettings =
  return $ clearSession cookieSettings $ VisitorSession { _sessionAccountId = 1
                                                        , _sessionCsrfToken = ""
                                                        }

-- * sign up

signUpHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => SignUp
  -> m ()
signUpHandler (SignUp { _signUpEmail }) =
  let
    CaseInsensitive email =
      _signUpEmail

    invalidEmail :: Bool
    invalidEmail =
      isLeft $ Email.validate (BSU.fromString email)

    ioEmailAlreadyUsed :: IO Bool
    ioEmailAlreadyUsed = do
      (getDBConnection >>= selectAccountFromEmail _signUpEmail) <&> isJust
  in
    if invalidEmail then
      throwError err400
    else do
      emailAlreadyUsed <- liftIO ioEmailAlreadyUsed
      if emailAlreadyUsed then
        throwError err400
      else do
        case mkHailgunMessage (mkSignUpEmail email) of
          Left error -> do
            liftIO $ print error
            throwError err400

          Right message -> do
            hailgunContext <- mkHailgunContext
            _ <- liftIO $ sendEmail hailgunContext message
            return ()

emailCtx :: EmailCtx
emailCtx =
  EmailCtx { _emailDomain = ""
           , _emailApiKey = ""
           }

mkSignUpEmail :: String -> Email
mkSignUpEmail _ =
  Email { _emailSubject = "coucou"
        , _emailMessageContent = "inscris toi mon coco"
        , _emailRecipients = ["matsuhar@gmail.com"]
        }

selectAccountFromEmail :: CaseInsensitive -> Connection -> IO (Maybe Account)
selectAccountFromEmail email connection = do
  (query connection selectAccountQuery (Only email)) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          |]
