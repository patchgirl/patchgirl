{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Environment.App where

import           DB
import           GHC.Generics
import           Data.Aeson (ToJSON, FromJSON)
import           Servant (Handler)
import           Database.PostgreSQL.Simple (Connection, Only(..), query)
import           Control.Monad.IO.Class (liftIO)
import           Database.PostgreSQL.Simple.SqlQQ

-- * Model

data UpdateEnvironment
  = UpdateEnvironment deriving (Eq, Show, Generic, FromJSON)

data Environment
  = Environment deriving (Eq, Show, Generic, ToJSON)

data NewEnvironment
  = NewEnvironment { name :: String
                   } deriving (Eq, Show, Generic, FromJSON)


-- * Create environment

insertEnvironment :: NewEnvironment -> Connection -> IO Int
insertEnvironment (NewEnvironment { name }) connection = do
  [Only id] <- query connection insertEnvironmentQuery $ (Only name)
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
