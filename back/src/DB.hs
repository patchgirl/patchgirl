{-# LANGUAGE FlexibleContexts #-}

module DB where

import           Config
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Functor               ((<&>))
import           Data.Text                  as TS
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Natural                (naturalToInt)

getDBConnection
  :: ( MonadReader Config m
     , MonadIO m
     )
  => m PG.Connection
getDBConnection = do
  DBConfig {..} <- ask <&> dbConfig
  liftIO $ PG.connect PG.defaultConnectInfo { PG.connectDatabase = TS.unpack dbName
                                            , PG.connectUser = TS.unpack dbUser
                                            , PG.connectPort = fromInteger $ toInteger $ naturalToInt dbPort
                                            , PG.connectPassword = TS.unpack dbPassword
                                            }
