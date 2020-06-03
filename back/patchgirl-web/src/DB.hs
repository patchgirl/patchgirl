
module DB where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Functor               ((<&>))
import           Data.Text                  as TS
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Natural                (naturalToInt)

import           PatchGirl.Internal.Env

getDBConnection
  :: ( MonadReader Env m
     , MonadIO m
     )
  => m PG.Connection
getDBConnection = do
  DBConfig {..} <- ask <&> _envDB
  liftIO $ PG.connect PG.defaultConnectInfo { PG.connectDatabase = TS.unpack _dbName
                                            , PG.connectUser = TS.unpack _dbUser
                                            , PG.connectPort = fromInteger $ toInteger $ naturalToInt _dbPort
                                            , PG.connectPassword = TS.unpack _dbPassword
                                            }
