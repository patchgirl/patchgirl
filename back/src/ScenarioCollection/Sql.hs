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
