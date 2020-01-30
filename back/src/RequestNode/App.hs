{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestNode.App where

import           Control.Lens.Operators ((^.))
import qualified Control.Monad.Except   as Except (MonadError)
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
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
  -> Int
  -> UpdateRequestNode
  -> m ()
updateRequestNodeHandler accountId _ requestNodeId updateRequestNode = do
  connection <- getDBConnection
  IO.liftIO (selectRequestCollectionId accountId connection) >>= \case
    Nothing ->
      Servant.throwError Servant.err404
    Just requestCollectionId -> do
      requestNodes <- IO.liftIO $ selectRequestNodesFromRequestCollectionId requestCollectionId connection
      case any (isNodeContainedInRequestNode requestNodeId) requestNodes of
        False -> Servant.throwError Servant.err404
        True ->
          IO.liftIO $
            updateRequestNodeDB requestNodeId updateRequestNode connection >> return ()

isNodeContainedInRequestNode :: Int -> RequestNode -> Bool
isNodeContainedInRequestNode nodeId requestNode =
  requestNode ^. requestNodeId == nodeId || case requestNode of
    RequestFile {} ->
      False
    RequestFolder {} ->
      any (isNodeContainedInRequestNode nodeId) (requestNode ^. requestNodeChildren)


-- * create request file


createRequestFileHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
  -> Int
  -> NewRequestFile
  -> m Int
createRequestFileHandler _ _ newRequestFile = do
  connection <- getDBConnection
  IO.liftIO $ insertRequestFile newRequestFile connection
