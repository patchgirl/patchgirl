module PatchGirl.Web.Account.Sql where

import           Data.Functor                     ((<&>))
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.Id

-- * select account from github id


selectAccountFromGithubId :: Int -> PG.Connection -> IO (Maybe (Id Account))
selectAccountFromGithubId githubId connection =
  PG.query connection selectAccountQuery (PG.Only githubId) <&> \case
    [PG.Only accountId] -> Just $ Id accountId
    _ -> Nothing
  where
    selectAccountQuery =
      [sql|
          SELECT id
          FROM account
          WHERE github_id = ?
          |]


-- * insert account


insertAccount :: Int -> PG.Connection -> IO (Id Account)
insertAccount githubId connection = do
  [PG.Only accountId] <- PG.query connection rawQuery (PG.Only githubId)
  return $ Id accountId
  where
    rawQuery =
      [sql|
          WITH new_account as (
            INSERT INTO account (github_id)
            VALUES (?)
            RETURNING id
          ), new_request_collection as (
            INSERT INTO request_collection(account_id)
            (SELECT id FROM new_account)
          ), new_scenario_collection as (
            INSERT INTO scenario_collection(account_id)
            (SELECT id FROM new_account)
          ), new_pg_collection as (
            INSERT INTO pg_collection (account_id)
            (SELECT id FROM new_account)
          )
          SELECT id FROM new_account
          |]
