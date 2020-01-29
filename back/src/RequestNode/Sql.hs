{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestNode.Sql where

import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           RequestNode.Model


selectRequestNodesFromRequestCollectionId :: Int -> PG.Connection -> IO [RequestNode]
selectRequestNodesFromRequestCollectionId requestCollectionId connection = do
    idToMRequestNodesFromPG <- PG.query connection selectRequestNodeSql (PG.Only requestCollectionId) :: IO[(Int, RequestNodeFromPG)]
    return $ map (fromPgRequestNodeToRequestNode . snd) idToMRequestNodesFromPG
  where
    selectRequestNodeSql =
      [sql|
          SELECT 1, UNNEST(root_request_nodes_as_json(?));
          |] :: PG.Query
