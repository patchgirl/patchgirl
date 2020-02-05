{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}


module RequestCollection.Sql where

import           Data.Functor                     ((<&>))
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ


selectRequestCollectionAvailable :: Int -> Int -> Connection -> IO Bool
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

selectRequestCollectionId :: Int -> Connection -> IO (Maybe Int)
selectRequestCollectionId accountId connection =
  query connection requestCollectionSql (Only accountId) <&> \case
    [Only id] -> Just id
    _ -> Nothing
  where
    requestCollectionSql =
      [sql|
          SELECT rc.id
          FROM request_collection rc
          WHERE rc.account_id = ?
          LIMIT 1
          |]
