{-# LANGUAGE FlexibleContexts #-}

module PatchGirl.Web.ScenarioCollection.App where

import           Control.Monad.Except                   (MonadError)
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Reader                   (MonadReader)
import           Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.Id
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.ScenarioCollection.Model
import           PatchGirl.Web.ScenarioCollection.Sql
import           PatchGirl.Web.ScenarioNode.Sql


-- * handler


getScenarioCollectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => Id Account
  -> m ScenarioCollection
getScenarioCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectScenarioCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404

    Just scenarioCollectionId -> do
      scenarioNodes <- liftIO $ selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection
      return $ ScenarioCollection scenarioCollectionId scenarioNodes
