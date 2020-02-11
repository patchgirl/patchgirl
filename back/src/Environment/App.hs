{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Environment.App where


-- * import


import           Control.Lens                     hiding (element)

import           Control.Lens                     (makeLenses)
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Data.Aeson                       (FromJSON, ToJSON (..),
                                                   defaultOptions,
                                                   fieldLabelModifier,
                                                   genericToJSON, parseJSON)
import           Data.Aeson.Types                 (genericParseJSON)
import           Data.Foldable                    (foldl')
import           Data.HashMap.Strict              as HashMap (HashMap, elems,
                                                              empty, insertWith)
import           Data.List                        (find)
import           Database.PostgreSQL.Simple       (Connection, FromRow,
                                                   Only (..), execute, query)
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           PatchGirl
import           Prelude                          hiding (id)
import           Servant                          (err404, throwError)
import           Servant.Server                   (ServerError)


-- * get environments


data PGEnvironmentWithKeyValue =
  PGEnvironmentWithKeyValue { _pgEnvironmentWithKeyValueEnvironmentId   :: Int
                            , _pgEnvironmentWithKeyValueEnvironmentName :: String
                            , _pgEnvironmentWithKeyValueKeyValueId      :: Int
                            , _pgEnvironmentWithKeyValueKey             :: String
                            , _pgEnvironmentWithKeyValueValue           :: String
                            } deriving (Generic, FromRow)

data PGEnvironmentWithoutKeyValue =
  PGEnvironmentWithoutKeyValue { _pgEnvironmentWithoutKeyValueEnvironmentId   :: Int
                               , _pgEnvironmentWithoutKeyValueEnvironmentName :: String
                               } deriving (Generic, FromRow)


$(makeLenses ''PGEnvironmentWithKeyValue)
$(makeLenses ''PGEnvironmentWithoutKeyValue)

data KeyValue =
  KeyValue { _keyValueId    :: Int
           , _keyValueKey   :: String
           , _keyValueValue :: String
           } deriving (Eq, Show, Generic, FromRow)

instance FromJSON KeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON KeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''KeyValue)

data Environment
  = Environment { _environmentId        :: Int
                , _environmentName      :: String
                , _environmentKeyValues :: [KeyValue]
                } deriving (Eq, Show, Generic)

instance FromJSON Environment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON Environment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''Environment)

selectEnvironments :: Int -> Connection -> IO [Environment]
selectEnvironments accountId connection = do
  pgEnvironmentsWithKeyValue :: [PGEnvironmentWithKeyValue] <- query connection selectEnvironmentQueryWithKeyValues (Only accountId)
  pgEnvironmentsWithoutKeyValues :: [PGEnvironmentWithoutKeyValue] <- query connection selectEnvironmentQueryWithoutKeyValues (Only accountId)

  let
    environmentsWithKeyValues =
      elems $ convertPgEnvironmentsToHashMap pgEnvironmentsWithKeyValue
    environmentsWithoutKeyValues =
      map convertPGEnviromentWithoutKeyValuesToEnvironment pgEnvironmentsWithoutKeyValues
    in
    return $ environmentsWithKeyValues ++ environmentsWithoutKeyValues
  where
    convertPgEnvironmentsToHashMap :: [PGEnvironmentWithKeyValue] -> HashMap Int Environment
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
            value
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
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> m [Environment]
getEnvironmentsHandler accountId = do
  connection <- getDBConnection
  liftIO $ selectEnvironments accountId connection


-- * create environment


newtype NewEnvironment
  = NewEnvironment { _newEnvironmentName :: String
                   } deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''NewEnvironment)

insertEnvironment :: NewEnvironment -> Connection -> IO Int
insertEnvironment NewEnvironment { _newEnvironmentName } connection = do
  [Only id] <- query connection insertEnvironmentQuery (Only _newEnvironmentName)
  return id
  where
    insertEnvironmentQuery =
      [sql|
          INSERT INTO environment (
            name
          )
          VALUES (?)
          RETURNING id
          |]

bindEnvironmentToAccount :: Int -> Int -> Connection -> IO ()
bindEnvironmentToAccount accountId environmentId connection = do
  _ <- execute connection bindEnvironmentToAccountQuery (accountId, environmentId)
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
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> NewEnvironment
  -> m Int
createEnvironmentHandler accountId newEnvironment = do
  connection <- getDBConnection
  environmentId <- liftIO $ insertEnvironment newEnvironment connection
  liftIO $
    bindEnvironmentToAccount accountId environmentId connection >> return environmentId


-- * update environment


newtype UpdateEnvironment
  = UpdateEnvironment { _name :: String }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdateEnvironment)

instance FromJSON UpdateEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

updateEnvironmentHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> UpdateEnvironment
  -> m ()
updateEnvironmentHandler accountId environmentId updateEnvironment = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  case environmentId `elem` map _environmentId environments  of
    False -> throwError err404
    True ->
      liftIO $ updateEnvironmentDB environmentId updateEnvironment connection

updateEnvironmentDB :: Int -> UpdateEnvironment -> Connection -> IO ()
updateEnvironmentDB environmentId UpdateEnvironment { _name } connection = do
  _ <- execute connection updateEnvironmentQuery (_name, environmentId)
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
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> m ()
deleteEnvironmentHandler accountId environmentId = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  case environmentId `elem` map _environmentId environments  of
    False -> throwError err404
    True ->
      liftIO $ deleteEnvironmentDB environmentId connection

deleteEnvironmentDB :: Int -> Connection -> IO ()
deleteEnvironmentDB environmentId connection = do
  _ <- execute connection deleteEnvironmentQuery $ Only environmentId
  return ()
  where
    deleteEnvironmentQuery =
      [sql|
          DELETE FROM environment
          WHERE id = ?
          |]


-- * delete key value


deleteKeyValueHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> Int
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

deleteKeyValueDB :: Int -> Connection -> IO ()
deleteKeyValueDB keyValueId connection = do
  _ <- execute connection deleteKeyValueQuery $ Only keyValueId
  return ()
  where
    deleteKeyValueQuery =
      [sql|
          DELETE FROM key_value
          WHERE id = ?
          |]


-- * upsert key values


-- ** model


data NewKeyValue
  = NewKeyValue { _newKeyValueKey   :: String
                , _newKeyValueValue :: String
                }
  deriving (Eq, Show, Generic)

instance FromJSON NewKeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewKeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


$(makeLenses ''NewKeyValue)


-- ** handler


deleteKeyValuesDB :: Int -> Connection -> IO ()
deleteKeyValuesDB environmentId' connection = do
  _ <- execute connection deleteKeyValuesQuery (Only environmentId')
  return ()
  where
    deleteKeyValuesQuery =
      [sql|
          DELETE FROM key_value
          WHERE environment_id = ?
          |]

insertManyKeyValuesDB :: Int -> NewKeyValue -> Connection -> IO KeyValue
insertManyKeyValuesDB environmentId NewKeyValue {..} connection = do
  [keyValue] <- query connection insertKeyValueQuery (environmentId, _newKeyValueKey, _newKeyValueValue)
  return keyValue
  where
    insertKeyValueQuery =
      [sql|
          INSERT INTO key_value (environment_id, key, value)
          VALUES (?, ?, ?)
          RETURNING id, key, value
          |]


updateKeyValuesHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> [NewKeyValue]
  -> m [KeyValue]
updateKeyValuesHandler accountId environmentId' newKeyValues = do
  connection <- getDBConnection
  environments <- liftIO $ selectEnvironments accountId connection
  let
    environment = find (\env -> env ^. environmentId == environmentId') environments
  case environment of
    Just _ -> do
      liftIO $ deleteKeyValuesDB environmentId' connection
      liftIO $ mapM (flip (insertManyKeyValuesDB environmentId') connection) newKeyValues
    Nothing ->
      throwError err404
