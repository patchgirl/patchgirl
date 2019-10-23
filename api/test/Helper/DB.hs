{-# LANGUAGE OverloadedStrings #-}

module Helper.DB (cleanDBAfter) where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import DB (getDBConnection)
import Data.Text (Text)
import           Control.Exception                (finally)
import           Control.Monad                    (void)

cleanDBAfter :: (Connection -> IO a) -> IO a
cleanDBAfter f =
  getDBConnection >>= withConnection f

withConnection :: (Connection -> IO a) -> Connection -> IO a
withConnection f connection =
  finally (f connection) $ listTables connection >>= mapM_ (truncateTable connection)

listTables :: Connection -> IO [Text]
listTables c =
  map fromOnly `fmap` query_ c q
  where
    q = mconcat [ "SELECT c.relname FROM pg_catalog.pg_class c"
                , " LEFT JOIN pg_catalog.pg_namespace n"
                , " ON c.relnamespace = n.oid"
                , " WHERE c.relkind IN ('r', '')"
                , " AND n.nspname <> 'pg_catalog'"
                , " AND n.nspname <> 'information_schema'"
                , " AND n.nspname !~ '^pg_toast'"
                , " AND pg_catalog.pg_table_is_visible(c.oid)"
                ]

truncateTable :: Connection -> Text -> IO ()
truncateTable c =
  void . execute c q . Only . Identifier
  where
    q = "TRUNCATE ? CASCADE"
