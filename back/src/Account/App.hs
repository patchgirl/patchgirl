{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module Account.App where

import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.Trans              (liftIO)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           PatchGirl
import           Servant.Server                   (ServerError)


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
            github_id,
            email,
            password
          ) values (
            '00000000-0000-1000-a000-000000000000',
            0,
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
