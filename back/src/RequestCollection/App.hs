{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RequestCollection.App where

import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (MonadReader)
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           PatchGirl
import           RequestNode.Model
import           Servant
import           Session.Model


-- * Model


data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)


-- * DB


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

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
    [(_, mRequestNodesFromPG)] <- query connection selectRequestCollectionSql (Only requestCollectionId) :: IO[(Int, Maybe [RequestNodeFromPG])]
    case mRequestNodesFromPG of
      Just requestNodesFromPG ->
        return . Just $ RequestCollection requestCollectionId (fromPgRequestNodeToRequestNode <$> requestNodesFromPG)
      Nothing ->
        return . Just $ RequestCollection requestCollectionId []
  where
    selectRequestCollectionSql =
      [sql|
          SELECT 1, *
          FROM request_nodes_as_json(?)
          |] :: Query


-- * Handler


getRequestCollectionHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> m RequestCollection
getRequestCollectionHandler accountId requestCollectionId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionAvailable accountId requestCollectionId connection) >>= \case
    False -> throwError err404
    True ->
      liftIO (selectRequestCollectionById requestCollectionId connection) >>= \case
        Just request -> return request
        Nothing      -> throwError err404
