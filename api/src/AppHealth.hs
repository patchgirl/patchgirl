{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AppHealth where

import           Config
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant


-- * Model

data AppHealth =
  AppHealth { isAppRunning :: Bool
            , isDBUp       :: Bool
            }
  deriving (Eq, Show, Generic)

instance ToJSON AppHealth
instance FromJSON AppHealth

-- * DB

selectDBIsRunning :: Connection -> IO Bool
selectDBIsRunning connection = do
  [Only isDBUp] <- query_ connection rawQuery
  return isDBUp
  where
    rawQuery =
      [sql|
          SELECT true
          |]

-- * Handler

getAppHealth
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => m AppHealth
getAppHealth = do
  connection <- getDBConnection
  liftIO $ selectDBIsRunning connection >>= \case
    True -> return $
      AppHealth { isAppRunning = True
                , isDBUp = True
                }
    False -> return $
      AppHealth { isAppRunning = True
                , isDBUp = False
                }
