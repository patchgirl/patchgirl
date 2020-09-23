
module PatchGirl.Web.RequestNode.App ( updateRequestNodeHandler
                       , deleteRequestNodeHandler
                       , createRootRequestFileHandler
                       , createRequestFileHandler
                       , updateRequestFileHandler
                       , createRootRequestFolderHandler
                       , createRequestFolderHandler
                       , findNodeInRequestNodes
                       ) where

import           Control.Lens.Operators ((^.))
import qualified Control.Monad          as Monad
import Data.Functor ((<&>))
import qualified Control.Monad.Except   as Except (MonadError)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import qualified Data.Maybe             as Maybe
import qualified Database.PostgreSQL.Simple       as PG
import           Data.UUID
import qualified Servant

import           PatchGirl.Web.DB
import           PatchGirl.Web.PatchGirl
import           PatchGirl.Web.RequestCollection.Sql
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.RequestNode.Sql


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
updateRequestNodeHandler accountId requestCollectionId requestNodeId updateRequestNode = do
  connection <- getDBConnection
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId requestNodeId $ \_ _ ->
      IO.liftIO $ Monad.void (updateRequestNodeDB requestNodeId updateRequestNode connection)


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
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId requestNodeId $ \_ _ ->
      IO.liftIO $ deleteRequestNodeDB requestNodeId connection


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
  ifValidRequestCollection connection accountId requestCollectionId $ do
    IO.liftIO $ insertRootRequestFile newRootRequestFile requestCollectionId connection


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
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId (newRequestFile ^. newRequestFileParentNodeId) $ \_ -> \case
      RequestFolder {} ->
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
updateRequestFileHandler accountId requestCollectionId requestNodeId updateRequestFile = do
  connection <- getDBConnection
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId requestNodeId $ \_ _ ->
      IO.liftIO $ Monad.void (updateRequestFileDB requestNodeId updateRequestFile connection)


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
  ifValidRequestCollection connection accountId requestCollectionId $ do
    IO.liftIO $ insertRootRequestFolder newRootRequestFolder requestCollectionId connection


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
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId (newRequestFolder ^. newRequestFolderParentNodeId) $ \_ -> \case
      RequestFolder {} ->
        IO.liftIO $ insertRequestFolder newRequestFolder connection
      _ ->
        Servant.throwError Servant.err404


-- * duplicate node


duplicateNodeHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => UUID
  -> Int
  -> UUID
  -> Maybe UUID
  -> m ()
duplicateNodeHandler accountId requestCollectionId origNodeId mFolderTargetNodeId = do
  connection <- getDBConnection
  ifValidRequestCollection connection accountId requestCollectionId $ do
    ifValidRequestNode connection requestCollectionId origNodeId $ \nodes node -> do
      let copy = node { _requestNodeName = _requestNodeName node ++ " copy" }
      maybe mFolderTargetNodeId (duplicateToRoot connection requestCollectionId copy) $ \targetFolderId ->
        case findNodeInRequestNodes targetFolderId nodes of
          Just RequestFolder {} ->
            duplicateToTargetNodeId connection requestCollectionId targetFolderId copy
          _ ->
            Servant.throwError Servant.err404
  where
    maybe m f g = Maybe.maybe f g m


-- * duplicate to root


duplicateToRoot
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => PG.Connection
  -> Int
  -> RequestNode
  -> m ()
duplicateToRoot connection requestCollectionId = \case
  RequestFolder {..} -> do
    let newRootRequestFolder = NewRootRequestFolder { _newRootRequestFolderId = _requestNodeId
                                                    , _newRootRequestFolderName = _requestNodeName
                                                    }
    IO.liftIO $ insertRootRequestFolder newRootRequestFolder requestCollectionId connection
    Monad.forM_ _requestNodeChildren $ duplicateToTargetNodeId connection requestCollectionId _requestNodeId


  RequestFile {..} -> do
    let newRootRequestFile =
          NewRootRequestFile { _newRootRequestFileId      = _requestNodeId
                             , _newRootRequestFileName    = _requestNodeName
                             , _newRootRequestFileHttpUrl = _requestNodeHttpUrl
                             , _newRootRequestFileMethod  = _requestNodeHttpMethod
                             , _newRootRequestFileHeaders = _requestNodeHttpHeaders <&> HttpHeader
                             , _newRootRequestFileBody    = _requestNodeHttpBody
                             }
    IO.liftIO $ insertRootRequestFile newRootRequestFile requestCollectionId connection


-- * duplicate to target


duplicateToTargetNodeId
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => PG.Connection
  -> Int
  -> UUID
  -> RequestNode
  -> m ()
duplicateToTargetNodeId connection requestCollectionId targetFolderId  = \case
  RequestFolder {..} -> do
    let newRequestFolder = NewRequestFolder { _newRequestFolderId = _requestNodeId
                                            , _newRequestFolderParentNodeId = targetFolderId
                                            , _newRequestFolderName = _requestNodeName
                                            }
    IO.liftIO $ insertRequestFolder newRequestFolder connection
    Monad.forM_ _requestNodeChildren $ duplicateToTargetNodeId connection requestCollectionId _requestNodeId

  RequestFile {..} -> do
    let newRequestFile =
          NewRequestFile { _newRequestFileId      = _requestNodeId
                         , _newRequestFileParentNodeId = targetFolderId
                         , _newRequestFileName    = _requestNodeName
                         , _newRequestFileHttpUrl = _requestNodeHttpUrl
                         , _newRequestFileMethod  = _requestNodeHttpMethod
                         , _newRequestFileHeaders = _requestNodeHttpHeaders <&> HttpHeader
                         , _newRequestFileBody    = _requestNodeHttpBody
                         }
    IO.liftIO $ insertRequestFile newRequestFile connection


-- * util


ifValidRequestCollection
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => PG.Connection
  -> UUID
  -> Int
  -> m ()
  -> m ()
ifValidRequestCollection connection accountId requestCollectionId f =
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId' | requestCollectionId /= requestCollectionId' ->
      Servant.throwError Servant.err404
    _  -> f

ifValidRequestNode
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => PG.Connection
  -> Int
  -> UUID
  -> ([RequestNode] -> RequestNode -> m a)
  -> m a
ifValidRequestNode connection requestCollectionId nodeId f = do
  nodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
  case findNodeInRequestNodes nodeId nodes of
    Nothing ->
      Servant.throwError Servant.err404

    Just node ->
      f nodes node

findNodeInRequestNodes :: UUID -> [RequestNode] -> Maybe RequestNode
findNodeInRequestNodes nodeIdToFind requestNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findNodeInRequestNode requestNodes)
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
