{-# LANGUAGE FlexibleContexts  #-}

module PatchGirl.Web.RequestCollection.App where

import           Control.Monad.Except    (MonadError)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader)
import           Data.UUID               (UUID)
import           Servant

import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestCollection.Sql
import           PatchGirl.Web.RequestNode.Sql
import           PatchGirl.Web.DB


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
