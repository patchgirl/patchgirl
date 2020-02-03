{-# LANGUAGE FlexibleContexts #-}

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
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
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



-- * create request file


createRequestFileHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
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

-- * create request folder


createRequestFolderHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
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


-- * delete request node


deleteRequestNodeHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
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
