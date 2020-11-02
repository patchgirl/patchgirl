
module PatchGirl.Web.ScenarioNode.App where

import qualified Control.Exception                    as Exception
import qualified Control.Monad                        as Monad
import qualified Control.Monad.Except                 as Except (MonadError)
import qualified Control.Monad.IO.Class               as IO
import qualified Control.Monad.Loops                  as Loops
import qualified Control.Monad.Reader                 as Reader
import           Data.Function                        ((&))
import           Data.Functor                         ((<&>))
import qualified Data.List                            as List
import qualified Data.Maybe                           as Maybe
import           Data.UUID
import qualified Database.PostgreSQL.Simple           as PG
import qualified Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.Id
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.PgCollection.Sql
import           PatchGirl.Web.PgNode.App
import           PatchGirl.Web.PgNode.Sql
import           PatchGirl.Web.RequestCollection.Sql
import           PatchGirl.Web.RequestNode.App
import           PatchGirl.Web.RequestNode.Sql
import           PatchGirl.Web.ScenarioCollection.Sql
import           PatchGirl.Web.ScenarioNode.Model
import           PatchGirl.Web.ScenarioNode.Sql


-- * update scenario node


updateScenarioNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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


-- * create root scenario file


createRootScenarioFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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
        Maybe.maybe False isScenarioFolder . findNodeInScenarioNodes (_newScenarioFileParentNodeId newScenarioFile)
  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ insertScenarioFile newScenarioFile connection


-- * update scenario file


updateScenarioFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
  -> UUID
  -> UpdateScenarioFile
  -> m ()
updateScenarioFileHandler accountId scenarioCollectionId updateScenarioFile = do
  connection <- getDBConnection
  let
    scenarioCollectionAuthorized :: IO Bool
    scenarioCollectionAuthorized = IO.liftIO $
      doesScenarioCollectionBelongsToAccount accountId scenarioCollectionId connection

    scenarioNodeAuthorized :: IO Bool
    scenarioNodeAuthorized =
        selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection <&>
        Maybe.maybe False isScenarioFile . findNodeInScenarioNodes (_updateScenarioFileId updateScenarioFile)

  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True -> do
      eRes <- IO.liftIO $ Exception.try $ updateScenarioFileDB updateScenarioFile accountId connection
      case eRes of
        Left PG.SqlError{} -> Servant.throwError Servant.err404
        Right _            -> return ()


-- * create root scenario folder


createRootScenarioFolderHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
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
        Maybe.maybe False isScenarioFolder . findNodeInScenarioNodes (_newScenarioFolderParentNodeId newScenarioFolder)

  authorized <- IO.liftIO $ Loops.andM [ scenarioCollectionAuthorized, scenarioNodeAuthorized ]
  case authorized of
    False ->
      Servant.throwError Servant.err404

    True ->
      IO.liftIO . Monad.void $ insertScenarioFolder newScenarioFolder connection


-- * create scene


createSceneHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
  -> UUID
  -> NewScene
  -> m ()
createSceneHandler accountId scenarioNodeId newScene = do
  connection <- getDBConnection
  mScenarioNode <- IO.liftIO $
    selectScenarioNodesFromAccountId accountId connection <&> findNodeInScenarioNodes scenarioNodeId

  let
    requestAuthorized :: IO Bool
    requestAuthorized = IO.liftIO $
      selectRequestCollectionId accountId connection >>= \case
        Nothing -> pure False
        Just collectionId -> do
          selectRequestNodesFromRequestCollectionId collectionId connection
            <&> findNodeInRequestNodes (Id $ _newSceneActorId newScene)
            <&> Maybe.isJust

    pgAuthorized :: IO Bool
    pgAuthorized = IO.liftIO $
      selectPgCollectionId accountId connection >>= \case
        Nothing -> pure False
        Just collectionId -> do
          selectPgNodesFromPgCollectionId collectionId connection
            <&> findNodeInPgNodes (_newSceneActorId newScene)
            <&> Maybe.isJust

    sceneAuthorized :: Bool
    sceneAuthorized =
      case mScenarioNode of
        Just ScenarioFile { _scenarioNodeScenes } ->
          case _newSceneSceneActorParentId newScene  of
            Just sceneNodeParentId -> do
              List.find (\scene -> sceneNodeParentId == _sceneId scene) _scenarioNodeScenes
                & Maybe.isJust
            Nothing -> True
        _ -> False

  authorized <- IO.liftIO $ Loops.andM [ Loops.orM [ requestAuthorized, pgAuthorized ]
                                       , pure sceneAuthorized
                                       ]
  case authorized of
    False ->
      Servant.throwError Servant.err404
    True ->
      IO.liftIO . Monad.void $ insertScene scenarioNodeId newScene connection


-- * delete scene


deleteSceneHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
  -> UUID
  -> UUID
  -> m ()
deleteSceneHandler accountId scenarioNodeId sceneId' = do
  connection <- getDBConnection
  IO.liftIO (selectScenarioCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404

    Just scenarioCollectionId -> do
      scenarioNodes <- IO.liftIO $ selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection
      let
        sceneAuthorized =
          findNodeInScenarioNodes scenarioNodeId scenarioNodes >>= \case
            ScenarioFolder {} -> Nothing
            ScenarioFile { _scenarioNodeScenes } ->
              List.find (\scene -> _sceneId scene  == sceneId') _scenarioNodeScenes

      case sceneAuthorized of
        Nothing ->
          Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO . Monad.void $ deleteScene sceneId' connection


-- * update scene


updateSceneHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Id Account
  -> UUID
  -> UUID
  -> UpdateScene
  -> m ()
updateSceneHandler accountId scenarioNodeId sceneId' updateScene = do
  connection <- getDBConnection
  IO.liftIO (selectScenarioCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404

    Just scenarioCollectionId -> do
      scenarioNodes <- IO.liftIO $ selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection
      let
        sceneAuthorized =
          findNodeInScenarioNodes scenarioNodeId scenarioNodes >>= \case
            ScenarioFile { _scenarioNodeScenes } ->
              List.find (\scene -> _sceneId scene == sceneId') _scenarioNodeScenes
            _ ->
              Nothing

      case sceneAuthorized of
        Nothing ->
          Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO . Monad.void $ updateSceneDB sceneId' updateScene connection




-- * util


isScenarioFolder :: ScenarioNode -> Bool
isScenarioFolder = \case
  ScenarioFolder {} -> True
  _ -> False

isScenarioFile :: ScenarioNode -> Bool
isScenarioFile = not . isScenarioFolder

findNodeInScenarioNodes :: UUID -> [ScenarioNode] -> Maybe ScenarioNode
findNodeInScenarioNodes nodeIdToFind scenarioNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findNodeInScenarioNode scenarioNodes)
  where
    findNodeInScenarioNode :: ScenarioNode -> Maybe ScenarioNode
    findNodeInScenarioNode scenarioNode =
      case _scenarioNodeId scenarioNode  == nodeIdToFind of
        True -> Just scenarioNode
        False ->
          case scenarioNode of
            ScenarioFile {} ->
              Nothing
            ScenarioFolder {} ->
              findNodeInScenarioNodes nodeIdToFind (_scenarioNodeChildren scenarioNode)
