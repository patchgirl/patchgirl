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

import           Data.Functor                     ((<&>))
import           PatchGirl
import           RequestCollection.Model
import           RequestNode.Sql
import           Servant


-- * handler


getRequestCollectionHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> m RequestCollection
getRequestCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404
    Just requestCollectionId -> do
      requestNodes <- liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      return $ RequestCollection requestCollectionId requestNodes


-- * db


selectRequestCollectionId :: Int -> Connection -> IO (Maybe Int)
selectRequestCollectionId accountId connection =
  query connection requestCollectionSql (Only accountId) <&> \case
    [Only id] -> Just id
    _ -> Nothing
  where
    requestCollectionSql =
      [sql|
          SELECT rc.id
          FROM request_collection2 rc
          INNER JOIN request_collection_to_request_node2 rcrn ON rc.id = rcrn.request_collection_id
          WHERE rc.account_id = ?
          LIMIT 1
          |]
