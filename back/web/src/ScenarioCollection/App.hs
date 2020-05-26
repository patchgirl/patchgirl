{-# LANGUAGE FlexibleContexts  #-}

module ScenarioCollection.App where

import           Control.Monad.Except     (MonadError)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader)
import           Data.UUID                (UUID)
import           DB

import           PatchGirl
import           ScenarioCollection.Model
import           ScenarioCollection.Sql
import           ScenarioNode.Sql
import           Servant


-- * handler


getScenarioCollectionHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => UUID
  -> m ScenarioCollection
getScenarioCollectionHandler accountId = do
  connection <- getDBConnection
  liftIO (selectScenarioCollectionId accountId connection) >>= \case
    Nothing ->
      throwError err404

    Just scenarioCollectionId -> do
      scenarioNodes <- liftIO $ selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection
      return $ ScenarioCollection scenarioCollectionId scenarioNodes
