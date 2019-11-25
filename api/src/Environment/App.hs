{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes       #-}
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

import Control.Lens hiding (element)

import Prelude hiding (id)
import           DB
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON, fieldLabelModifier, genericToJSON, defaultOptions, parseJSON)
import           Data.Aeson.Types (genericParseJSON)
import           Servant (Handler)
import           Database.PostgreSQL.Simple (Connection, Only(..), query, FromRow, execute)
import           Control.Monad.IO.Class (liftIO)
import           Database.PostgreSQL.Simple.SqlQQ
import  Data.HashMap.Strict as HashMap (HashMap, empty, insertWith, elems)
import  Data.Foldable (foldl')

-- * Model

data UpdateEnvironment
  = UpdateEnvironment deriving (Eq, Show, Generic, FromJSON)

data Environment
  = Environment { _id :: Int
                , _name :: String
                , _keyValues :: [KeyValue]
                } deriving (Eq, Show, Generic)

instance ToJSON Environment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

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


data NewEnvironment
  = NewEnvironment { _name :: String
                   } deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data KeyValue =
  KeyValue { _id :: Int
           , _key :: String
           , _value :: String
           } deriving (Eq, Show, Generic)

instance ToJSON KeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''KeyValue)
$(makeFieldsNoPrefix ''Environment)
$(makeFieldsNoPrefix ''NewEnvironment)
$(makeFieldsNoPrefix ''PGEnvironmentWithKeyValue)
$(makeFieldsNoPrefix ''PGEnvironmentWithoutKeyValue)

-- * Get environments

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

-- * Create environment

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

-- * else

updateEnvironmentHandler = undefined
deleteEnvironmentHandler = undefined
