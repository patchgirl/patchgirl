module DB where

import qualified Database.PostgreSQL.Simple as PG

getDBConnection :: IO PG.Connection
getDBConnection = PG.connect PG.defaultConnectInfo
  { PG.connectDatabase = "test"
  , PG.connectUser = "test"
  , PG.connectPort = 5433
  , PG.connectPassword = "test"

  }
