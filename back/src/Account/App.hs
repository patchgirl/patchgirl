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
signUpHandler SignUp {..} =
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
mkSignUpEmail CreatedAccount {..} =
  let CaseInsensitive email = _accountCreatedEmail
  in
    Email { _emailSubject = "Finish your signing up"
          , _emailTextMessageContent =
            "Howdy! You're almost done. Finalize your subscription by setting your password here: https://patchgirl.io/#account/" <> show _accountCreatedId <> "/initializePassword/" <> _accountCreatedSignUpToken
          , _emailHtmlMessageContent =
            "Howdy!<br/> You're almost done. Finalize your subscription by setting your password <a href=\"https://patchgirl.io/#account/" <> show _accountCreatedId <> "/initializePassword/" <> _accountCreatedSignUpToken <> "\">here.</a>"
          , _emailRecipients = [email]
          }

selectAccountFromEmail :: CaseInsensitive -> PG.Connection -> IO (Maybe Account)
selectAccountFromEmail email connection =
  PG.query connection selectAccountQuery (PG.Only email) <&> listToMaybe
  where
    selectAccountQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE email = ?
          |]

insertAccount :: NewAccount -> PG.Connection -> IO CreatedAccount
insertAccount NewAccount {..} connection = do
  [accountCreated] <- PG.query connection rawQuery (PG.Only _newAccountEmail)
  return accountCreated
  where
    rawQuery =
      [sql|
          WITH new_account as (
            INSERT INTO account (email)
            VALUES (?)
            RETURNING id, email, signup_token
          ), new_request_collection as (
            INSERT INTO request_collection(account_id)
            (SELECT id FROM new_account)
          )
          SELECT id, email, signup_token
          FROM new_account
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

selectAccountFromInitializePassword :: InitializePassword -> PG.Connection -> IO (Maybe Account)
selectAccountFromInitializePassword InitializePassword {..} connection =
  PG.query connection selectAccountQuery ( _initializePasswordAccountId
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

setPassword :: InitializePassword -> PG.Connection -> IO ()
setPassword InitializePassword {..} connection = do
  _ <- PG.execute connection selectAccountQuery ( _initializePasswordPassword
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


-- * reset visitor account


resetVisitorAccountHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => m ()
resetVisitorAccountHandler = do
  connection <- getDBConnection
  liftIO $ resetVisitorSql connection

resetVisitorSql :: PG.Connection -> IO ()
resetVisitorSql connection = do
  _ <- PG.execute_ connection resetVisitorQuery
  return ()
  where
    resetVisitorQuery =
      [sql|


-- ** delete former visitor data


          -- delete visitor account
          DELETE FROM account WHERE id = '00000000-0000-1000-a000-000000000000';

          -- delete orphan environment
          DELETE FROM environment
          WHERE id IN (
            SELECT id FROM environment
            LEFT JOIN account_environment ON id = environment_id
            WHERE environment_id IS NULL
          );

          -- delete orphan request_node
          DELETE FROM request_node
          WHERE id IN (
            SELECT id
            FROM request_node
            LEFT JOIN request_collection_to_request_node ON id = request_node_id
            WHERE request_node_id IS NULL
          );


-- ** insert account


          -- insert visitor account
          INSERT INTO account (
            id,
            email,
            password
          ) values (
            '00000000-0000-1000-a000-000000000000',
            'visitor@patchgirl.io',
            crypt('123', gen_salt('bf', 8))
          );


-- ** requests


-- *** users/


          INSERT INTO request_node (id, request_node_parent_id, tag, name)
          VALUES ('58954f35-49ac-45b7-bcf6-c8df1af4b12c', NULL, 'RequestFolder', 'users');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('e46ee2de-f1ce-4b13-b1ec-b529ae87da54', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'list users', 'https://{{host}}/users', 'Get', ARRAY[('key1','value1')]::header_type[], '');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('e5324e42-76e5-4fa4-8243-0348dba8c1a8', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'single user', 'https://{{host}}/users/2', 'Get', ARRAY[]::header_type[], '');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('5ff67d3c-28a2-4aa1-b474-4b10dabd2852', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'create user', 'https://{{host}}/users', 'Post', ARRAY[('key1','value1')]::header_type[], '');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('718a67f1-9ff2-4d09-a14a-1b9f4c029a26', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'update user', 'https://{{host}}/users/2', 'Put', ARRAY[]::header_type[], '');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('913d508c-fef3-4034-98da-9e328debb196', '58954f35-49ac-45b7-bcf6-c8df1af4b12c', 'RequestFile', 'delete user', 'https://{{host}}/users/2', 'Delete', ARRAY[]::header_type[], '');


-- *** session/

          INSERT INTO request_node (id, request_node_parent_id, tag, name)
          VALUES ('da0a3654-5e30-471f-ba03-f87760976981', NULL, 'RequestFolder', 'session');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('b3b24406-a7c0-4c68-bdcc-279e843340a0', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'login successful', 'https://{{host}}/login', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '{
  "email": "{{user}}@reqres.in",
  "password": "cityslicka"
}');

          INSERT INTO request_node (id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body)
          VALUES ('6a55626d-d1ec-4255-851d-2b8e18f4bdc4', 'da0a3654-5e30-471f-ba03-f87760976981', 'RequestFile', 'login unsuccessful', 'https://{{host}}/login', 'Post', ARRAY[('Content-Type','application/json')]::header_type[], '');


-- ** request collection


          INSERT INTO request_collection (id, account_id)
          VALUES (1, '00000000-0000-1000-a000-000000000000');

          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (1,'58954f35-49ac-45b7-bcf6-c8df1af4b12c');

          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (1,'da0a3654-5e30-471f-ba03-f87760976981');


-- ** environment


          INSERT INTO environment (id, name)
          VALUES (1, 'prod');

          INSERT INTO environment (id, name)
          VALUES (2, 'dev');

          INSERT INTO account_environment (account_id, environment_id)
          VALUES ('00000000-0000-1000-a000-000000000000', 1);

          INSERT INTO account_environment (account_id, environment_id)
          VALUES ('00000000-0000-1000-a000-000000000000', 2);


-- ** key values


          INSERT INTO key_value (environment_id, key, value)
          VALUES (1, 'host', 'reqres.in/api');

          INSERT INTO key_value (environment_id, key, value)
          VALUES (1, 'user', 'eve.holt');

          INSERT INTO key_value ( environment_id, key, value)
          VALUES (2, 'host', 'reqres.in/api');

          INSERT INTO key_value ( environment_id, key, value)
          VALUES ( 2, 'user', 'whatever');

          |]
