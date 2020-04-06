{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}


module ScenarioCollection.Sql where

import           Data.Functor                     ((<&>))
import           Data.UUID                        (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ


-- * does scenario collection belongs to account


doesScenarioCollectionBelongsToAccount :: UUID -> UUID -> Connection -> IO Bool
doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection =
  query connection scenarioCollectionSql (accountId, scenarioCollectionId) <&> \case
    [Only True] -> True
    _ -> False
  where
    scenarioCollectionSql =
      [sql|
          SELECT EXISTS(
            SELECT id
            FROM scenario_collection
            WHERE account_id = ?
            AND id = ?
            LIMIT 1
          )
          |]


-- * select scenario collection id


selectScenarioCollectionId :: UUID -> Connection -> IO (Maybe UUID)
selectScenarioCollectionId accountId connection =
  query connection scenarioCollectionSql (Only accountId) <&> \case
    [Only id] -> Just id
    _ -> Nothing
  where
    scenarioCollectionSql =
      [sql|
          SELECT id
          FROM scenario_collection
          WHERE account_id = ?
          LIMIT 1
          |]
