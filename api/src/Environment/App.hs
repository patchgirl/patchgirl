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
import Control.Lens.TH

import Prelude hiding (id)
import           DB
import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON, fieldLabelModifier, genericToJSON, defaultOptions)
import           Servant (Handler)
import           Database.PostgreSQL.Simple (Connection, Only(..), query, FromRow)
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

data PGEnvironment =
  PGEnvironment { _environmentName :: String
                , _environmentId :: Int
                , _keyValueId :: Int
                , _key :: String
                , _value :: String
                } deriving (Generic, FromRow)

data NewEnvironment
  = NewEnvironment { _name :: String
                   } deriving (Eq, Show, Generic, FromJSON)

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
$(makeFieldsNoPrefix ''PGEnvironment)

-- * Get environments

selectEnvironments :: Connection -> IO [Environment]
selectEnvironments connection = do
  pgEnvironments :: [PGEnvironment] <- query connection selectEnvironmentQuery $ (Only 1 :: Only Int)
  return $ elems $ convertPgEnvironmentsToHashMap pgEnvironments
  where
    convertPgEnvironmentsToHashMap :: [PGEnvironment] -> HashMap Int Environment
    convertPgEnvironmentsToHashMap pgEnvironments =
      foldl' (\acc pgEnv -> insertWith mergeValue (pgEnv ^. environmentId) (convertPGEnviromentToEnvironment pgEnv) acc) HashMap.empty pgEnvironments

    convertPGEnviromentToEnvironment :: PGEnvironment -> Environment
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

    mergeValue :: Environment -> Environment -> Environment
    mergeValue oldEnv newEnv =
      oldEnv & keyValues %~ (++) (newEnv ^. keyValues)

    selectEnvironmentQuery =
      [sql|
          SELECT
            environment.name as environment_name,
            environment.id as environment_id,
            key_value.id as key_value_id,
            key,
            value
          FROM key_value
          JOIN environment ON (key_value.environment_id = environment.id)
          JOIN account_environment ON (account_environment.environment_id = environment.id)
          WHERE account_id = ?;
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

createEnvironmentHandler :: NewEnvironment -> Handler Int
createEnvironmentHandler newEnvironment = do
  liftIO (getDBConnection >>= (insertEnvironment newEnvironment)) >>= return

-- * else

updateEnvironmentHandler = undefined
deleteEnvironmentHandler = undefined
