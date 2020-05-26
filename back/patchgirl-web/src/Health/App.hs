{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Health.App where

import           Env
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics


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
  [Only dbUp] <- query_ connection rawQuery
  return dbUp
  where
    rawQuery =
      [sql|
          SELECT true
          |]


-- * Handler


getAppHealthHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => m AppHealth
getAppHealthHandler = do
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
