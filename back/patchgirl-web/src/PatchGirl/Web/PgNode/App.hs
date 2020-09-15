
module PatchGirl.Web.PgNode.App ( updatePgNodeHandler
                  , deletePgNodeHandler
                  , createRootPgFileHandler
                  , createPgFileHandler
                  , updatePgFileHandler
                  , createRootPgFolderHandler
                  , createPgFolderHandler
                  , findNodeInPgNodes
                  ) where

import           Control.Lens.Operators ((^.))
import qualified Control.Monad          as Monad
import qualified Control.Monad.Except   as Except (MonadError)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import qualified Data.Maybe             as Maybe
import           Data.UUID
import qualified Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.PgCollection.Sql
import           PatchGirl.Web.PgNode.Model
import           PatchGirl.Web.PgNode.Sql


-- * update pg node


updatePgNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> UpdatePgNode
  -> m ()
updatePgNodeHandler accountId _ pgNodeId updatePgNode = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just pgCollectionId -> do
      pgNodes <- IO.liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      case findNodeInPgNodes pgNodeId pgNodes of
        Nothing -> Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO $
            Monad.void (updatePgNodeDB pgNodeId updatePgNode connection)


-- * delete pg node


deletePgNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> m ()
deletePgNodeHandler accountId pgCollectionId pgNodeId = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just pgCollectionId' | pgCollectionId /= pgCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      pgNodes <- IO.liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      case findNodeInPgNodes pgNodeId pgNodes of
        Nothing ->
          Servant.throwError Servant.err404
        _ ->
          IO.liftIO $ deletePgNodeDB pgNodeId connection


-- * util


findNodeInPgNodes :: UUID -> [PgNode] -> Maybe PgNode
findNodeInPgNodes nodeIdToFind pgNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findNodeInPgNode pgNodes)
  where
    findNodeInPgNode :: PgNode -> Maybe PgNode
    findNodeInPgNode pgNode =
      case pgNode ^. pgNodeId == nodeIdToFind of
        True -> Just pgNode
        False ->
          case pgNode of
            PgFile {} ->
              Nothing
            PgFolder {} ->
              findNodeInPgNodes nodeIdToFind (pgNode ^. pgNodeChildren)


-- * create root pg file


createRootPgFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewRootPgFile
  -> m ()
createRootPgFileHandler accountId pgCollectionId newRootPgFile = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Just pgCollectionId' | pgCollectionId == pgCollectionId' -> do
      IO.liftIO $ insertRootPgFile newRootPgFile pgCollectionId connection
    _ ->
      Servant.throwError Servant.err404


-- * create pg file


createPgFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewPgFile
  -> m ()
createPgFileHandler accountId pgCollectionId newPgFile = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just pgCollectionId' | pgCollectionId /= pgCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      pgNodes <- IO.liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      case findNodeInPgNodes (newPgFile ^. newPgFileParentNodeId) pgNodes of
        Just PgFolder {} ->
          IO.liftIO $ insertPgFile newPgFile connection
        _ ->
          Servant.throwError Servant.err404


-- * update pg file


updatePgFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> UUID
  -> UpdatePgFile
  -> m ()
updatePgFileHandler accountId _ pgNodeId updatePgFile = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just pgCollectionId -> do
      pgNodes <- IO.liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      case findNodeInPgNodes pgNodeId pgNodes of
        Nothing -> Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO $
            Monad.void (updatePgFileDB pgNodeId updatePgFile connection)


-- * create root pg folder


createRootPgFolderHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewRootPgFolder
  -> m ()
createRootPgFolderHandler accountId pgCollectionId newRootPgFolder = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Just pgCollectionId' | pgCollectionId == pgCollectionId' ->
      IO.liftIO $ insertRootPgFolder newRootPgFolder pgCollectionId connection
    _ ->
      Servant.throwError Servant.err404


-- * create pg folder


createPgFolderHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> UUID
  -> NewPgFolder
  -> m ()
createPgFolderHandler accountId pgCollectionId newPgFolder = do
  connection <- getDBConnection
  IO.liftIO (selectPgCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just pgCollectionId' | pgCollectionId /= pgCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      pgNodes <- IO.liftIO $ selectPgNodesFromPgCollectionId pgCollectionId connection
      case findNodeInPgNodes (newPgFolder ^. newPgFolderParentNodeId) pgNodes of
        Just PgFolder {} -> do
          IO.liftIO $ insertPgFolder newPgFolder connection
        _ ->
          Servant.throwError Servant.err404
