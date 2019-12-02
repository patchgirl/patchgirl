{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Environment.App where

-- * import

import Control.Lens hiding (element)

import Data.List (find)
import Prelude hiding (id)
import           DB
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON, fieldLabelModifier, genericToJSON, defaultOptions, parseJSON)
import           Data.Aeson.Types (genericParseJSON)
import           Servant (Handler, throwError, err404)
import           Database.PostgreSQL.Simple (Connection, Only(..), query, FromRow, execute)
import           Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import           Control.Monad.IO.Class (liftIO)
import           Database.PostgreSQL.Simple.SqlQQ
import  Data.HashMap.Strict as HashMap (HashMap, empty, insertWith, elems)
import  Data.Foldable (foldl')
import Control.Lens (makeFieldsNoPrefix)

-- * get environments


data PGEnvironmentWithKeyValue =
  PGEnvironmentWithKeyValue { _environmentId :: Int
                            , _environmentName :: String
                            , _keyValueId :: Int
                            , _key :: String
                            , _value :: String
                            } deriving (Generic, FromRow)

data PGEnvironmentWithoutKeyValue =
  PGEnvironmentWithoutKeyValue { _environmentId :: Int
                               , _environmentName :: String
                               } deriving (Generic, FromRow)


$(makeFieldsNoPrefix ''PGEnvironmentWithKeyValue)
$(makeFieldsNoPrefix ''PGEnvironmentWithoutKeyValue)

data KeyValue =
  KeyValue { _id :: Int
           , _key :: String
           , _value :: String
           } deriving (Eq, Show, Generic, FromRow)

instance ToJSON KeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''KeyValue)

data Environment
  = Environment { _id :: Int
                , _name :: String
                , _keyValues :: [KeyValue]
                } deriving (Eq, Show, Generic)

instance ToJSON Environment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''Environment)

selectEnvironments :: Connection -> IO [Environment]
selectEnvironments connection = do
  pgEnvironmentsWithKeyValue :: [PGEnvironmentWithKeyValue] <- query connection selectEnvironmentQueryWithKeyValues $ (Only 1 :: Only Int)
  pgEnvironmentsWithoutKeyValues :: [PGEnvironmentWithoutKeyValue] <- query connection selectEnvironmentQueryWithoutKeyValues $ (Only 1 :: Only Int)

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
      foldl' (\acc pgEnv -> insertWith mergeValue (pgEnv ^. environmentId) (convertPGEnviromentToEnvironment pgEnv) acc) HashMap.empty pgEnvironments

    convertPGEnviromentToEnvironment :: PGEnvironmentWithKeyValue -> Environment
    convertPGEnviromentToEnvironment pgEnv =
      let
        keyValue :: KeyValue
        keyValue =
          KeyValue { _id = pgEnv ^. keyValueId
                   , _key = pgEnv ^. key
                   , _value = pgEnv ^. value
                   }
      in
        Environment { _id = pgEnv ^. environmentId
                    , _name = pgEnv ^. environmentName
                    , _keyValues = [ keyValue ]
                    }

    convertPGEnviromentWithoutKeyValuesToEnvironment :: PGEnvironmentWithoutKeyValue -> Environment
    convertPGEnviromentWithoutKeyValuesToEnvironment pgEnv =
        Environment { _id = pgEnv ^. environmentId
                    , _name = pgEnv ^. environmentName
                    , _keyValues = []
                    }

    mergeValue :: Environment -> Environment -> Environment
    mergeValue oldEnv newEnv =
      oldEnv & keyValues %~ (++) (newEnv ^. keyValues)

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

getEnvironmentsHandler :: Handler [Environment]
getEnvironmentsHandler = do
  liftIO (getDBConnection >>= selectEnvironments >>= return)


-- * create environment


data NewEnvironment
  = NewEnvironment { _name :: String
                   } deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''NewEnvironment)

insertEnvironment :: NewEnvironment -> Connection -> IO Int
insertEnvironment (NewEnvironment { _name }) connection = do
  [Only id] <- query connection insertEnvironmentQuery $ (Only _name)
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
  _ <- execute connection bindEnvironmentToAccountQuery $ (accountId, environmentId)
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

createEnvironmentHandler :: NewEnvironment -> Handler Int
createEnvironmentHandler newEnvironment = do
  connection <- liftIO getDBConnection
  environmentId <- liftIO $ insertEnvironment newEnvironment connection
  liftIO $ bindEnvironmentToAccount 1 environmentId connection >> return environmentId

-- * update environment


data UpdateEnvironment
  = UpdateEnvironment { _name :: String }
  deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''UpdateEnvironment)

instance FromJSON UpdateEnvironment where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

updateEnvironmentHandler :: Int -> UpdateEnvironment -> Handler ()
updateEnvironmentHandler environmentId updateEnvironment = do
  liftIO (getDBConnection >>= (updateEnvironmentDB environmentId updateEnvironment))

updateEnvironmentDB :: Int -> UpdateEnvironment -> Connection -> IO ()
updateEnvironmentDB environmentId (UpdateEnvironment { _name }) connection = do
  _ <- execute connection updateEnvironmentQuery $ (_name, environmentId)
  return ()
  where
    updateEnvironmentQuery =
      [sql|
          UPDATE environment
          SET name = ?
          WHERE id = ?
          |]


-- * delete environment


deleteEnvironmentHandler :: Int -> Handler ()
deleteEnvironmentHandler environmentId =
  liftIO (getDBConnection >>= (deleteEnvironmentDB environmentId))

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


deleteKeyValueHandler :: Int -> Int -> Handler ()
deleteKeyValueHandler environmentId keyValueId = do
  connection <- liftIO getDBConnection
  environments <- liftIO $ selectEnvironments connection
  let
    mKeyValue = do
      environment <- find (\environment -> environment ^. id == environmentId) environments
      find (\keyValue -> keyValue ^. id == keyValueId) $ environment ^. keyValues
  case mKeyValue of
    Just keyValue ->
      liftIO $ deleteKeyValueDB (keyValue ^. id) connection
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
  = NewKeyValue { _newKeyValueKey :: String
                , _newKeyValueValue :: String
                }
  deriving (Eq, Show, Generic)

instance ToField NewKeyValue where
  toField (NewKeyValue { _newKeyValueKey, _newKeyValueValue }) =
    Many [ toField _newKeyValueKey
         , toField _newKeyValueValue
         ]

instance FromJSON NewKeyValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''NewKeyValue)

-- ** handler

insertManyKeyValuesDB :: Int -> NewKeyValue -> Connection -> IO KeyValue
insertManyKeyValuesDB environmentId newKeyValue connection = do
  [keyValue] <- query connection insertKeyValueQuery $ (environmentId, newKeyValue)
  return keyValue
  where
    insertKeyValueQuery =
      [sql|
          INSERT INTO key_value (environment_id, key, value)
          VALUES (?, ?, ?)
          RETURNING id, key, value
          |]


updateKeyValuesHandler :: Int -> [NewKeyValue] -> Handler [KeyValue]
updateKeyValuesHandler environmentId newKeyValues = do
  connection <- liftIO getDBConnection
  environments <- liftIO $ selectEnvironments connection
  let
    environment = find (\environment -> environment ^. id == environmentId) environments
  case environment of
    Just _ -> do
      liftIO $ deleteEnvironmentDB environmentId connection
      liftIO $ mapM (flip (insertManyKeyValuesDB environmentId) connection) newKeyValues
    Nothing ->
      throwError err404
