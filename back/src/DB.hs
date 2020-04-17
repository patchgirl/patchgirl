
module DB where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Functor               ((<&>))
import           Data.Text                  as TS
import qualified Database.PostgreSQL.Simple as PG
import           Env
import           GHC.Natural                (naturalToInt)

getDBConnection
  :: ( MonadReader Env m
     , MonadIO m
     )
  => m PG.Connection
getDBConnection = do
  DBConfig {..} <- ask <&> envDB
  liftIO $ PG.connect PG.defaultConnectInfo { PG.connectDatabase = TS.unpack dbName
                                            , PG.connectUser = TS.unpack dbUser
                                            , PG.connectPort = fromInteger $ toInteger $ naturalToInt dbPort
                                            , PG.connectPassword = TS.unpack dbPassword
                                            }
