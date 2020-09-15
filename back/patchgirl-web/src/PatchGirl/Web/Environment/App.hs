{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module PatchGirl.Web.Environment.App where


-- * import


import           Control.Lens                     ((&))
import           Control.Lens.Getter              ((^.))
import qualified Control.Monad          as Monad
import           Control.Lens.Setter              ((%~))
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Data.Foldable                    (foldl')
import           Data.HashMap.Strict              as HashMap (HashMap, elems,
                                                              empty, insertWith)
import           Data.List                        (find)
import           Data.UUID                        (UUID)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           Environment.Model
import           PatchGirl
import           Prelude                          hiding (id)
import           Servant                          (err404, throwError)
import           Servant.Server                   (ServerError)


-- * environment


selectEnvironments :: UUID -> PG.Connection -> IO [Environment]
selectEnvironments accountId connection = do
  pgEnvironmentsWithKeyValue :: [PGEnvironmentWithKeyValue] <- PG.query connection selectEnvironmentQueryWithKeyValues (PG.Only accountId)
  pgEnvironmentsWithoutKeyValues :: [PGEnvironmentWithoutKeyValue] <- PG.query connection selectEnvironmentQueryWithoutKeyValues (PG.Only accountId)

  let
    environmentsWithKeyValues =
      elems $ convertPgEnvironmentsToHashMap pgEnvironmentsWithKeyValue
    environmentsWithoutKeyValues =
      map convertPGEnviromentWithoutKeyValuesToEnvironment pgEnvironmentsWithoutKeyValues
    in
    return $ environmentsWithKeyValues ++ environmentsWithoutKeyValues
  where
    convertPgEnvironmentsToHashMap :: [PGEnvironmentWithKeyValue] -> HashMap UUID Environment
    convertPgEnvironmentsToHashMap pgEnvironments =
      foldl' (\acc pgEnv -> insertWith mergeValue (pgEnv ^. pgEnvironmentWithKeyValueEnvironmentId) (convertPGEnviromentToEnvironment pgEnv) acc) HashMap.empty pgEnvironments

    convertPGEnviromentToEnvironment :: PGEnvironmentWithKeyValue -> Environment
    convertPGEnviromentToEnvironment pgEnv =
      let
        keyValue :: KeyValue
        keyValue =
          KeyValue { _keyValueId = pgEnv ^. pgEnvironmentWithKeyValueKeyValueId
                   , _keyValueKey = pgEnv ^. pgEnvironmentWithKeyValueKey
                   , _keyValueValue = pgEnv ^. pgEnvironmentWithKeyValueValue
                   , _keyValueHidden = pgEnv ^. pgEnvironmentWithKeyValueHidden
                   }
      in
        Environment { _environmentId = pgEnv ^. pgEnvironmentWithKeyValueEnvironmentId
                    , _environmentName = pgEnv ^. pgEnvironmentWithKeyValueEnvironmentName
                    , _environmentKeyValues = [ keyValue ]
                    }

    convertPGEnviromentWithoutKeyValuesToEnvironment :: PGEnvironmentWithoutKeyValue -> Environment
    convertPGEnviromentWithoutKeyValuesToEnvironment pgEnv =
        Environment { _environmentId = pgEnv ^. pgEnvironmentWithoutKeyValueEnvironmentId
                    , _environmentName = pgEnv ^. pgEnvironmentWithoutKeyValueEnvironmentName
                    , _environmentKeyValues = []
                    }

    mergeValue :: Environment -> Environment -> Environment
    mergeValue oldEnv newEnv =
      oldEnv & environmentKeyValues %~ (++) (newEnv ^. environmentKeyValues)

    selectEnvironmentQueryWithKeyValues =
      [sql|
          SELECT
            environment.id as environment_id,
            environment.name as environment_name,
            key_value.id as key_value_id,
            key,
            value,
            hidden
          FROM key_value
          JOIN environment ON (key_value.environment_id = environment.id)
          JOIN account_environment ON (account_environment.environment_id = environment.id)
          WHERE account_id = ?;
          |]

    selectEnvironmentQueryWithoutKeyValues =
      [sql|
          SELECT
            environment.id as environment_id,
            environment.name as environment_name
          FROM environment
          LEFT JOIN key_value ON (key_value.environment_id = environment.id)
          JOIN account_environment ON (account_environment.environment_id = environment.id)
          WHERE key_value.environment_id IS NULL
          AND account_id = ?;
          |]


getEnvironmentsHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => UUID
  -> m [Environment]
getEnvironmentsHandler accountId = do
  connection <- getDBConnection
  liftIO $ selectEnvironments accountId connection


-- * create environment


insertEnvironment :: NewEnvironment -> PG.Connection -> IO UUID
insertEnvironment NewEnvironment {..} connection = do
  [PG.Only id] <- PG.query connection insertEnvironmentQuery (_newEnvironmentId, _newEnvironmentName)
  return id
  where
    insertEnvironmentQuery =
      [sql|
          INSERT INTO environment (
            id,
            name
          )
          VALUES (?, ?)
          RETURNING id
          |]

bindEnvironmentToAccount :: UUID -> UUID -> PG.Connection -> IO ()
bindEnvironmentToAccount accountId environmentId connection = do
  _ <- PG.execute connection bindEnvironmentToAccountQuery (accountId, environmentId)
  return ()
  where
    bindEnvironmentToAccountQuery =
      [sql|
          INSERT INTO account_environment (
            account_id,
            environment_id
          ) values (
            ?,
            ?
          );
          |]

createEnvironmentHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => UUID -> NewEnvironment -> m ()
createEnvironmentHandler accountId newEnvironment = do
  connection <- getDBConnection
  environmentId <- liftIO $ insertEnvironment newEnvironment connection
  liftIO $
    bindEnvironmentToAccount accountId environmentId connection


-- * update environment


updateEnvironmentHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID -> UUID -> UpdateEnvironment -> m ()
updateEnvironmentHandler accountId environmentId updateEnvironment = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  case environmentId `elem` map _environmentId environments  of
    False -> throwError err404
    True ->
      liftIO $ updateEnvironmentDB environmentId updateEnvironment connection

updateEnvironmentDB :: UUID -> UpdateEnvironment -> PG.Connection -> IO ()
updateEnvironmentDB environmentId UpdateEnvironment { _updateEnvironmentName } connection = do
  _ <- PG.execute connection updateEnvironmentQuery (_updateEnvironmentName, environmentId)
  return ()
  where
    updateEnvironmentQuery =
      [sql|
          UPDATE environment
          SET name = ?
          WHERE id = ?
          |]


-- * delete environment


deleteEnvironmentHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> UUID
  -> m ()
deleteEnvironmentHandler accountId environmentId = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  case environmentId `elem` map _environmentId environments  of
    False -> throwError err404
    True ->
      liftIO $ deleteEnvironmentDB environmentId connection

deleteEnvironmentDB :: UUID -> PG.Connection -> IO ()
deleteEnvironmentDB environmentId connection = do
  _ <- PG.execute connection deleteEnvironmentQuery $ PG.Only environmentId
  return ()
  where
    deleteEnvironmentQuery =
      [sql|
          DELETE FROM environment
          WHERE id = ?
          |]


-- * delete key value


deleteKeyValueHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> m ()
deleteKeyValueHandler accountId environmentId' keyValueId' = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  let
    mKeyValue = do
      environment <- find (\environment -> environment ^. environmentId == environmentId') environments
      find (\keyValue -> keyValue ^. keyValueId == keyValueId') $ environment ^. environmentKeyValues
  case mKeyValue of
    Just keyValue ->
      liftIO $ deleteKeyValueDB (keyValue ^. keyValueId) connection
    Nothing -> throwError err404

deleteKeyValueDB :: UUID -> PG.Connection -> IO ()
deleteKeyValueDB keyValueId connection = do
  _ <- PG.execute connection deleteKeyValueQuery $ PG.Only keyValueId
  return ()
  where
    deleteKeyValueQuery =
      [sql|
          DELETE FROM key_value
          WHERE id = ?
          |]


-- * upsert key values


deleteKeyValuesDB :: UUID -> PG.Connection -> IO ()
deleteKeyValuesDB environmentId' connection = do
  _ <- PG.execute connection deleteKeyValuesQuery (PG.Only environmentId')
  return ()
  where
    deleteKeyValuesQuery =
      [sql|
          DELETE FROM key_value
          WHERE environment_id = ?
          |]

insertManyKeyValuesDB :: UUID -> NewKeyValue -> PG.Connection -> IO KeyValue
insertManyKeyValuesDB environmentId NewKeyValue {..} connection = do
  [keyValue] <- PG.query connection insertKeyValueQuery (environmentId, _newKeyValueId, _newKeyValueKey, _newKeyValueValue, _newKeyValueHidden)
  return keyValue
  where
    insertKeyValueQuery =
      [sql|
          INSERT INTO key_value (environment_id, id, key, value, hidden)
          VALUES (?, ?, ?, ?, ?)
          RETURNING id, key, value, hidden
          |]

updateKeyValuesHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> UUID
  -> [NewKeyValue]
  -> m ()
updateKeyValuesHandler accountId environmentId' newKeyValues = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  let
    environment = find (\env -> env ^. environmentId == environmentId') environments
  case environment of
    Just _ -> do
      liftIO $ deleteKeyValuesDB environmentId' connection
      liftIO $ Monad.void $ mapM (flip (insertManyKeyValuesDB environmentId') connection) newKeyValues
    Nothing ->
      throwError err404
