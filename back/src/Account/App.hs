{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module Account.App where

import           Account.Model
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.Trans              (liftIO)
import           Data.ByteString.UTF8             as BSU
import           Data.Either                      (isLeft)
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (isJust, listToMaybe)
import           Database.PostgreSQL.Simple       (Connection, Only (..),
                                                   execute, query)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           Mailgun.App
import           Model
import           PatchGirl
import           Servant                          (err400, throwError)
import           Servant.Server                   (ServerError)
import           Session.Model
import qualified Text.Email.Validate              as Email


-- * sign up


signUpHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => SignUp
  -> m ()
signUpHandler SignUp { _signUpEmail } =
  let
    CaseInsensitive email =
      _signUpEmail

    malformedEmail :: Bool
    malformedEmail =
      isLeft $ Email.validate (BSU.fromString email)

    ioEmailAlreadyUsed :: PG.Connection -> IO Bool
    ioEmailAlreadyUsed connection =
      liftIO $ selectAccountFromEmail _signUpEmail connection <&> isJust

    invalidEmail :: PG.Connection -> IO Bool
    invalidEmail connection =
      if malformedEmail then pure True else ioEmailAlreadyUsed connection

  in do
    connection <- getDBConnection
    invalid <- liftIO $ invalidEmail connection
    case invalid of
      True -> throwError err400
      False -> do
        let newAccount = NewAccount { _newAccountEmail = _signUpEmail }
        createdAccount <- liftIO $ insertAccount newAccount connection
        hailgunMessage <- mkHailgunMessage (mkSignUpEmail createdAccount)
        case hailgunMessage of
          Left error -> do
            liftIO $ print error
            throwError err400

          Right message -> do
            hailgunContext <- mkHailgunContext
            emailRes <- liftIO $ sendEmail hailgunContext message
            case emailRes of
              Left error ->
                liftIO $ print error
              Right _    ->
                return ()


mkSignUpEmail :: CreatedAccount -> Email
mkSignUpEmail CreatedAccount { _accountCreatedId
                             , _accountCreatedEmail
                             , _accountCreatedSignUpToken
                             } =
  let CaseInsensitive email = _accountCreatedEmail
  in
    Email { _emailSubject = "Finish your signing up"
          , _emailTextMessageContent = "Howdy! You're almost done. Finalize your subscription by setting your password here: patchgirl.io/#account/" <> show _accountCreatedId <> "/initializePassword/" <> _accountCreatedSignUpToken
          , _emailHtmlMessageContent = "Howdy!<br/> You're almost done. Finalize your subscription by setting your password <a href=\"patchgirl.io/#account/" <> show _accountCreatedId <> "/initializePassword/" <> _accountCreatedSignUpToken <> "\">here.</a>"
          , _emailRecipients = [email]
          }

selectAccountFromEmail :: CaseInsensitive -> Connection -> IO (Maybe Account)
selectAccountFromEmail email connection =
  query connection selectAccountQuery (Only email) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          |]

insertAccount :: NewAccount -> Connection -> IO CreatedAccount
insertAccount NewAccount { _newAccountEmail } connection = do
  [accountCreated] <- query connection rawQuery (Only _newAccountEmail)
  return accountCreated
  where
    rawQuery =
      [sql|
          INSERT INTO account (email) VALUES (?)
          RETURNING id, email, signup_token
          |]


-- * initialize password


initializePasswordHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => InitializePassword
  -> m ()
initializePasswordHandler initializePassword = do
  connection <- getDBConnection
  mAccount <- liftIO $ selectAccountFromInitializePassword initializePassword connection
  case mAccount of
    Just _ -> do
      liftIO $ setPassword initializePassword connection
      return ()

    Nothing ->
      throwError err400

selectAccountFromInitializePassword :: InitializePassword -> Connection -> IO (Maybe Account)
selectAccountFromInitializePassword InitializePassword { _initializePasswordAccountId
                                                       , _initializePasswordToken
                                                       } connection =
  query connection selectAccountQuery ( _initializePasswordAccountId
                                      , _initializePasswordToken
                                      ) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE id = ?
          AND password IS NULL
          AND signup_token = ?
          |]

setPassword :: InitializePassword -> Connection -> IO ()
setPassword InitializePassword { _initializePasswordPassword
                               , _initializePasswordAccountId
                               } connection = do
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
