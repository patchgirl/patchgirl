{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PgCollection.Sql where

import           Data.Functor                     ((<&>))
import           Data.UUID                        (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ


-- * select pg collection available


selectPgCollectionAvailable :: UUID -> UUID -> Connection -> IO Bool
selectPgCollectionAvailable accountId pgCollectionId connection =
  query connection collectionExistsSql (pgCollectionId, accountId) >>= \case
    [Only True] -> return True
    _ -> return False
  where
    collectionExistsSql =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM pg_collection
            WHERE id = ?
            AND account_id = ?
          );
          |]


-- * select pg collection id


selectPgCollectionId :: UUID -> Connection -> IO (Maybe UUID)
selectPgCollectionId accountId connection =
  query connection pgCollectionSql (Only accountId) <&> \case
    [Only id] -> Just id
    _ -> Nothing
  where
    pgCollectionSql =
      [sql|
          SELECT id
          FROM pg_collection
          WHERE account_id = ?
          LIMIT 1
          |]
