{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PatchGirl.Web.PgCollection.Sql where

import           Data.Functor                     ((<&>))
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.Id
import           PatchGirl.Web.PgCollection.Model


-- * select pg collection available


selectPgCollectionAvailable :: Id Account -> Id PgCollection -> Connection -> IO Bool
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


selectPgCollectionId :: Id Account -> Connection -> IO (Maybe (Id PgCollection))
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
