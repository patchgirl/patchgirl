{-# LANGUAGE FlexibleContexts #-}

module PatchGirl.Web.PgCollection.App where

import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.Id
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.PgCollection.Model
import           PatchGirl.Web.PgCollection.Sql
import           PatchGirl.Web.PgNode.Sql


-- * handler


getPgCollectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Id Account
  -> m PgCollection
getPgCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404
    Just pgCollectionId -> do
      pgNodes <- liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      return $ PgCollection pgCollectionId pgNodes
