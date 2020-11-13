{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module PatchGirl.Web.Connection.App where


-- * import


import qualified Control.Monad                    as Monad
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Data.ByteString.UTF8             as BSU
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int                          (Int64)
import           Prelude                          hiding (id)
import           Servant                          (err404, throwError)
import           Servant.Server                   (ServerError)

import           PatchGirl.Web.Connection.Model
import           PatchGirl.Web.DB
import           PatchGirl.Web.Id
import           PatchGirl.Web.PatchGirl


-- * connection


getConnectionsHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => Id Account
  -> m [Connection]
getConnectionsHandler accountId = do
  connection <- getDBConnection
  liftIO $ selectConnections accountId connection


selectConnections :: Id Account -> PG.Connection -> IO [Connection]
selectConnections accountId connection = do
  PG.query connection query (PG.Only accountId)
  where
    query =
      [sql|
          SELECT
            id,
            name,
            tag,
            pg_host,
            pg_password,
            pg_port,
            pg_user,
            pg_db_name
          FROM connection
          WHERE account_id = ?
          |]





-- * create connection


createConnectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => Id Account -> NewConnection -> m ()
createConnectionHandler accountId newConnection = do
  connection <- getDBConnection
  liftIO $ Monad.void $ insertConnection newConnection accountId connection

insertConnection :: NewConnection -> Id Account -> PG.Connection -> IO Int64
insertConnection NewConnection {..} accountId connection = do
  let payload =
        ( _newConnectionId
        , accountId
        , _newConnectionName
        , _newConnectionTag
        , _newConnectionPgHost
        , _newConnectionPgPassword
        , _newConnectionPgPort
        , _newConnectionPgUser
        , _newConnectionPgDbName
        )
  PG.formatQuery connection insertConnectionQuery payload >>= putStrLn . BSU.toString
  PG.execute connection insertConnectionQuery payload
  where
    insertConnectionQuery =
      [sql|
          INSERT INTO connection (
            id,
            account_id,
            name,
            tag,
            pg_host,
            pg_password,
            pg_port,
            pg_user,
            pg_db_name
          )
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
          |]


-- * update connection


updateConnectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Id Account
  -> Id Con
  -> UpdateConnection
  -> m ()
updateConnectionHandler accountId connectionId updateConnection = do
  connection <- getDBConnection
  connections <- liftIO $ selectConnections accountId connection
  case connectionId `elem` map _connectionId connections  of
    False -> throwError err404
    True ->
      liftIO $ updateConnectionDB connectionId updateConnection connection

updateConnectionDB :: Id Con -> UpdateConnection -> PG.Connection -> IO ()
updateConnectionDB connectionId UpdateConnection { _updateConnectionName } connection = do
  _ <- PG.execute connection updateConnectionQuery (_updateConnectionName, connectionId)
  return ()
  where
    updateConnectionQuery =
      [sql|
          UPDATE connection
          SET name = ?
          WHERE id = ?
          |]


-- * delete connection


deleteConnectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Id Account
  -> Id Con
  -> m ()
deleteConnectionHandler accountId connectionId = do
  connection <- getDBConnection
  connections <- liftIO $ selectConnections accountId connection
  case connectionId `elem` map _connectionId connections  of
    False -> throwError err404
    True ->
      liftIO $ deleteConnectionDB connectionId connection

deleteConnectionDB :: Id Con -> PG.Connection -> IO ()
deleteConnectionDB connectionId connection = do
  _ <- PG.execute connection deleteConnectionQuery $ PG.Only connectionId
  return ()
  where
    deleteConnectionQuery =
      [sql|
          DELETE FROM connection
          WHERE id = ?
          |]
