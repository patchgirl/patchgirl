{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PatchGirl.Web.RequestCollection.Sql where

import           Data.Functor                     ((<&>))
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.Id


-- * select request collection available


selectRequestCollectionAvailable :: Id Account -> Int -> Connection -> IO Bool
selectRequestCollectionAvailable accountId requestCollectionId connection =
  query connection collectionExistsSql (requestCollectionId, accountId) >>= \case
    [Only True] -> return True
    _ -> return False
  where
    collectionExistsSql =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_collection
            WHERE id = ?
            AND account_id = ?
          );
          |]


-- * select request collection id


selectRequestCollectionId :: Id Account -> Connection -> IO (Maybe Int)
selectRequestCollectionId accountId connection =
  query connection requestCollectionSql (Only accountId) <&> \case
    [Only id] -> Just id
    _ -> Nothing
  where
    requestCollectionSql =
      [sql|
          SELECT id
          FROM request_collection
          WHERE account_id = ?
          LIMIT 1
          |]
