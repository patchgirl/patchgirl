{-# LANGUAGE FlexibleContexts  #-}

module PatchGirl.Web.RequestCollection.App where

import           Control.Monad.Except    (MonadError)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader)
import           Data.UUID               (UUID)
import           DB

import           PatchGirl
import           RequestCollection.Model
import           RequestCollection.Sql
import           RequestNode.Sql
import           Servant


-- * handler


getRequestCollectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> m RequestCollection
getRequestCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404
    Just requestCollectionId -> do
      requestNodes <- liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      return $ RequestCollection requestCollectionId requestNodes
