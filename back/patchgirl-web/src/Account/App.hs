{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module Account.App where

import qualified Control.Monad                    as Monad
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.Trans              (liftIO)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import qualified GHC.Int                          as Int
import           PatchGirl


-- * handler


resetVisitorAccountHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => m ()
resetVisitorAccountHandler = do
  connection <- getDBConnection
  liftIO $ Monad.void $ resetVisitorSql connection


-- * sql


resetVisitorSql :: PG.Connection -> IO Int.Int64
resetVisitorSql connection = do
  PG.execute_ connection [sql|


-- ** delete former visitor data


          -- delete visitor account
          DELETE FROM account WHERE id = '00000000-0000-1000-a000-000000000000';

          -- delete orphan scenario_node
          DELETE FROM scenario_node
          WHERE id IN (
            SELECT id
            FROM scenario_node
            LEFT JOIN scenario_collection_to_scenario_node ON id = scenario_node_id
            WHERE scenario_node_id IS NULL
          );

          -- delete orphan scene_node
          DELETE FROM scene_node
          WHERE id IN (
            SELECT scene_node.id
            FROM scene_node
            LEFT JOIN scenario_node ON scene_node.id = scene_node_id
            WHERE scenario_node.id IS NULL
          );

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

          -- delete orphan pg_node
          DELETE FROM pg_node
          WHERE id IN (
            SELECT id
            FROM pg_node
            LEFT JOIN pg_collection_to_pg_node ON id = pg_actor_id
            WHERE pg_actor_id IS NULL
          );


-- ** insert account


          -- insert visitor account
          INSERT INTO account (
            id,
            github_id,
            email
          ) values (
            '00000000-0000-1000-a000-000000000000',
            0,
            'visitor@patchgirl.io'
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


-- ** pg


          INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
          VALUES ('cb2c1df8-68f0-4a61-b7c7-f75194604976', NULL, 'PgFolder', 'users');

          INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
          VALUES ('0c37579e-6a6c-4e9f-ae2c-47a7e7270d14', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'all users',  'SELECT * FROM user_test;', 'localhost', '', '5432', '', '');

          INSERT INTO pg_node (id, pg_node_parent_id, tag, name, sql, pg_host, pg_password, pg_port, pg_user, pg_dbname)
          VALUES ('aa517710-150f-4707-a8cc-a24af252acd7', 'cb2c1df8-68f0-4a61-b7c7-f75194604976', 'PgFile', 'single users',  'SELECT * FROM user_test where id = 1 ;', 'localhost', '', '5432', '', '');


-- ** request collection


          INSERT INTO request_collection (id, account_id)
          VALUES (1, '00000000-0000-1000-a000-000000000000');

          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (1,'58954f35-49ac-45b7-bcf6-c8df1af4b12c');

          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (1,'da0a3654-5e30-471f-ba03-f87760976981');


-- ** pg collection


          INSERT INTO pg_collection (id, account_id)
          VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97', '00000000-0000-1000-a000-000000000000');

          INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
          VALUES ('d45a8a8d-c0a3-439d-ac65-3f2992e61b97','cb2c1df8-68f0-4a61-b7c7-f75194604976');


-- ** scenario


          INSERT INTO scenario_collection (account_id, id)
          VALUES ('00000000-0000-1000-a000-000000000000', 'a9e3fbc2-de07-40a5-afd8-2460ef1e202c');


-- ** environment


          INSERT INTO environment (id, name)
          VALUES ('98fa7543-aaaf-41a2-9b42-54129bd96551', 'prod');

          INSERT INTO environment (id, name)
          VALUES ('38668b92-647d-4108-92c8-b539fdc7a7bd', 'dev');

          INSERT INTO account_environment (account_id, environment_id)
          VALUES ('00000000-0000-1000-a000-000000000000', '98fa7543-aaaf-41a2-9b42-54129bd96551');

          INSERT INTO account_environment (account_id, environment_id)
          VALUES ('00000000-0000-1000-a000-000000000000', '38668b92-647d-4108-92c8-b539fdc7a7bd');


-- ** key values


          INSERT INTO key_value (id, environment_id, key, value, hidden)
          VALUES ('aea099bd-cbee-433c-a852-be9db08c607d', '98fa7543-aaaf-41a2-9b42-54129bd96551', 'host', 'reqres.in/api', true);

          INSERT INTO key_value (id, environment_id, key, value, hidden)
          VALUES ('05445705-8494-48ea-9535-616a7cfde992', '98fa7543-aaaf-41a2-9b42-54129bd96551', 'user', 'eve.holt', true);

          INSERT INTO key_value (id, environment_id, key, value, hidden)
          VALUES ('1b983dbb-26d8-4fa6-b152-941ea3b0f905', '38668b92-647d-4108-92c8-b539fdc7a7bd', 'host', 'reqres.in/api', true);

          INSERT INTO key_value (id, environment_id, key, value, hidden)
          VALUES ('e9279277-2215-411e-9198-af381744eb4a', '38668b92-647d-4108-92c8-b539fdc7a7bd', 'user', 'whatever', false);

          |]
