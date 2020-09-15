{-# LANGUAGE FlexibleContexts #-}

module PatchGirl.Web.PgCollection.App where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.UUID              (UUID)
import           DB

import           PatchGirl
import           PgCollection.Model
import           PgCollection.Sql
import           PgNode.Sql
import           Servant


-- * handler


getPgCollectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> m PgCollection
getPgCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404
    Just pgCollectionId -> do
      pgNodes <- liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      return $ PgCollection pgCollectionId pgNodes
