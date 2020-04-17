
module RequestNode.App where

import           Control.Lens.Operators ((^.))
import qualified Control.Monad          as Monad
import qualified Control.Monad.Except   as Except (MonadError)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import qualified Data.Maybe             as Maybe
import           Data.UUID
import qualified Servant

import           DB
import           PatchGirl
import           RequestCollection.Sql
import           RequestNode.Model
import           RequestNode.Sql


-- * update request node


updateRequestNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> UUID
  -> UpdateRequestNode
  -> m ()
updateRequestNodeHandler accountId _ requestNodeId updateRequestNode = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case findNodeInRequestNodes requestNodeId requestNodes of
        Nothing -> Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO $
            Monad.void (updateRequestNodeDB requestNodeId updateRequestNode connection)


-- * delete request node


deleteRequestNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> UUID
  -> m ()
deleteRequestNodeHandler accountId requestCollectionId requestNodeId = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId' | requestCollectionId /= requestCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case findNodeInRequestNodes requestNodeId requestNodes of
        Nothing ->
          Servant.throwError Servant.err404
        _ ->
          IO.liftIO $ deleteRequestNodeDB requestNodeId connection


-- * util


findNodeInRequestNodes :: UUID -> [RequestNode] -> Maybe RequestNode
findNodeInRequestNodes nodeIdToFind requestNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findNodeInRequestNode requestNodes
  where
    findNodeInRequestNode :: RequestNode -> Maybe RequestNode
    findNodeInRequestNode requestNode =
      case requestNode ^. requestNodeId == nodeIdToFind of
        True -> Just requestNode
        False ->
          case requestNode of
            RequestFile {} ->
              Nothing
            RequestFolder {} ->
              findNodeInRequestNodes nodeIdToFind (requestNode ^. requestNodeChildren)


-- * create root request file


createRootRequestFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> NewRootRequestFile
  -> m ()
createRootRequestFileHandler accountId requestCollectionId newRootRequestFile = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Just requestCollectionId' | requestCollectionId == requestCollectionId' ->
      IO.liftIO $ insertRootRequestFile newRootRequestFile requestCollectionId connection
    _ ->
      Servant.throwError Servant.err404


-- * create request file


createRequestFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> NewRequestFile
  -> m ()
createRequestFileHandler accountId requestCollectionId newRequestFile = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId' | requestCollectionId /= requestCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case findNodeInRequestNodes (newRequestFile ^. newRequestFileParentNodeId) requestNodes of
        Just RequestFolder {} ->
          IO.liftIO $ insertRequestFile newRequestFile connection
        _ ->
          Servant.throwError Servant.err404


-- * update request file


updateRequestFileHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> UUID
  -> UpdateRequestFile
  -> m ()
updateRequestFileHandler accountId _ requestNodeId updateRequestFile = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case findNodeInRequestNodes requestNodeId requestNodes of
        Nothing -> Servant.throwError Servant.err404
        Just _ ->
          IO.liftIO $
            Monad.void (updateRequestFileDB requestNodeId updateRequestFile connection)


-- * create root request folder


createRootRequestFolderHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> NewRootRequestFolder
  -> m ()
createRootRequestFolderHandler accountId requestCollectionId newRootRequestFolder = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Just requestCollectionId' | requestCollectionId == requestCollectionId' ->
      IO.liftIO $ insertRootRequestFolder newRootRequestFolder requestCollectionId connection
    _ ->
      Servant.throwError Servant.err404


-- * create request folder


createRequestFolderHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> NewRequestFolder
  -> m ()
createRequestFolderHandler accountId requestCollectionId newRequestFolder = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId' | requestCollectionId /= requestCollectionId' ->
      Servant.throwError Servant.err404
    _ -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case findNodeInRequestNodes (newRequestFolder ^. newRequestFolderParentNodeId) requestNodes of
        Just RequestFolder {} ->
          IO.liftIO $ insertRequestFolder newRequestFolder connection
        _ ->
          Servant.throwError Servant.err404
