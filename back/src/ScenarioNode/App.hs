{-# LANGUAGE FlexibleContexts #-}

module ScenarioNode.App where

import           Control.Lens.Operators ((^.))
import qualified Control.Monad          as Monad
import qualified Control.Monad.Except   as Except (MonadError)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops    as Loops
import qualified Control.Monad.Reader   as Reader
import           Data.Functor           ((<&>))
import qualified Data.Maybe             as Maybe
import           Data.UUID
import qualified Servant

import           DB
import           PatchGirl
import           ScenarioCollection.Sql
import           ScenarioNode.Model
import           ScenarioNode.Sql


-- * update scenario node


updateScenarioNodeHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> UpdateScenarioNode
  -> m ()
updateScenarioNodeHandler accountId scenarioCollectionId scenarioNodeId updateScenarioNode = do
  connection <- getDBConnection
  let scenarioCollectionAuthorized = doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  let scenarioNodeAuthorized =
        selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection <&>
        Maybe.isJust . findNodeInScenarioNodes scenarioNodeId
  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ updateScenarioNodeDB scenarioNodeId updateScenarioNode connection


-- * delete scenario node


deleteScenarioNodeHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> m ()
deleteScenarioNodeHandler accountId scenarioCollectionId scenarioNodeId = do
  connection <- getDBConnection
  let scenarioCollectionAuthorized = doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  let scenarioNodeAuthorized =
        selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection <&>
        Maybe.isJust . findNodeInScenarioNodes scenarioNodeId
  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ deleteScenarioNodeDB scenarioNodeId connection


-- * util


isScenarioFolder :: ScenarioNode -> Bool
isScenarioFolder = \case
  ScenarioFolder {} -> True
  _ -> False

findNodeInScenarioNodes :: UUID -> [ScenarioNode] -> Maybe ScenarioNode
findNodeInScenarioNodes nodeIdToFind scenarioNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findNodeInScenarioNode scenarioNodes
  where
    findNodeInScenarioNode :: ScenarioNode -> Maybe ScenarioNode
    findNodeInScenarioNode scenarioNode =
      case scenarioNode ^. scenarioNodeId == nodeIdToFind of
        True -> Just scenarioNode
        False ->
          case scenarioNode of
            ScenarioFile {} ->
              Nothing
            ScenarioFolder {} ->
              findNodeInScenarioNodes nodeIdToFind (scenarioNode ^. scenarioNodeChildren)


-- * create root scenario file


createRootScenarioFileHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewRootScenarioFile
  -> m ()
createRootScenarioFileHandler accountId scenarioCollectionId newRootScenarioFile = do
  connection <- getDBConnection
  scenarioCollectionAuthorized <- IO.liftIO $ doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  case scenarioCollectionAuthorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ insertRootScenarioFile newRootScenarioFile scenarioCollectionId connection


-- * create scenario file


createScenarioFileHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewScenarioFile
  -> m ()
createScenarioFileHandler accountId scenarioCollectionId newScenarioFile = do
  connection <- getDBConnection
  let scenarioCollectionAuthorized = IO.liftIO $ doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  let
      scenarioNodeAuthorized :: IO Bool
      scenarioNodeAuthorized =
        selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection <&>
        Maybe.maybe False isScenarioFolder . findNodeInScenarioNodes (newScenarioFile ^. newScenarioFileParentNodeId)
  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ insertScenarioFile newScenarioFile connection


-- * create root scenario folder


createRootScenarioFolderHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewRootScenarioFolder
  -> m ()
createRootScenarioFolderHandler accountId scenarioCollectionId newRootScenarioFolder = do
  connection <- getDBConnection
  scenarioCollectionAuthorized <- IO.liftIO $ doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  case scenarioCollectionAuthorized of
    False ->
      Servant.throwError Servant.err404

    True ->
      IO.liftIO . Monad.void $ insertRootScenarioFolder newRootScenarioFolder scenarioCollectionId connection



-- * create scenario folder


createScenarioFolderHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewScenarioFolder
  -> m ()
createScenarioFolderHandler accountId scenarioCollectionId newScenarioFolder = do
  connection <- getDBConnection
  let scenarioCollectionAuthorized = IO.liftIO $ doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection
  let
      scenarioNodeAuthorized :: IO Bool
      scenarioNodeAuthorized =
        selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection <&>
        Maybe.maybe False isScenarioFolder . findNodeInScenarioNodes (newScenarioFolder ^. newScenarioFolderParentNodeId)

  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404

    True ->
      IO.liftIO . Monad.void $ insertScenarioFolder newScenarioFolder connection
