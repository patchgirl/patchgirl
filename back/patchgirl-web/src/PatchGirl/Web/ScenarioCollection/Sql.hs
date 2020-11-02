{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PatchGirl.Web.ScenarioCollection.Sql where

import           Data.Functor                     ((<&>))
import           Data.UUID                        (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.Id


-- * does scenario collection belongs to account


doesScenarioCollectionBelongsToAccount :: Id Account -> UUID -> Connection -> IO Bool
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


selectScenarioCollectionId :: Id Account -> Connection -> IO (Maybe UUID)
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
