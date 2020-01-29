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

import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           DB

import           PatchGirl
import           RequestCollection.Model
import           RequestCollection.Sql
import           RequestNode.Sql
import           Servant


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


-- * handler


getRequestCollectionHandler2
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> m RequestCollection
getRequestCollectionHandler2 accountId requestCollectionId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionAvailable2 accountId requestCollectionId connection) >>= \case
    False -> throwError err404
    True -> do
      requestNodes <- liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      return $ RequestCollection requestCollectionId requestNodes


-- * db


selectRequestCollectionAvailable2 :: Int -> Int -> Connection -> IO Bool
selectRequestCollectionAvailable2 accountId requestCollectionId connection =
  query connection collectionExistsSql (accountId, requestCollectionId) >>= \case
    [Only True] -> return True
    _ -> return False
  where
    collectionExistsSql =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_collection2
            INNER JOIN request_collection_to_request_node2 ON id = request_collection_id
            WHERE account_id = ?
            AND request_collection_id = ?
          )
          |]
