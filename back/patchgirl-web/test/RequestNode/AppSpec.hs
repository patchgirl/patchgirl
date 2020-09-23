{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}



module RequestNode.AppSpec where

import           Control.Lens.Getter ((^.))
import           Data.UUID
import qualified Data.UUID           as UUID
import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import qualified Data.Maybe as Maybe
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Servant.Client      (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Server
import           PatchGirl.Web.Api
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model


-- * client


updateRequestNodeHandler :: Auth.Token -> Int -> UUID -> UpdateRequestNode -> ClientM ()
deleteRequestNodeHandler :: Auth.Token -> Int -> UUID -> ClientM ()
duplicateNodeHandler :: Auth.Token -> Int -> UUID -> DuplicateNode -> ClientM ()
updateRequestNodeHandler :<|> deleteRequestNodeHandler :<|> duplicateNodeHandler =
  client (Proxy :: Proxy (RequestNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec = focus $
  withClient (mkApp defaultEnv) $ do


-- ** update request node


    describe "update request node" $ do
      it "returns 404 when request node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (updateRequestNodeHandler token 1 UUID.nil updateRequestNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the request node doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId2 connection
          let nodeId = head requestNodes ^. requestNodeId
          try clientEnv (updateRequestNodeHandler token requestCollectionId nodeId updateRequestNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = head requestNodes ^. requestNodeId
          _ <- try clientEnv (updateRequestNodeHandler token requestCollectionId nodeId updateRequestNode)
          FakeRequestFolder { _fakeRequestFolderName } <- selectFakeRequestFolder nodeId connection
          _fakeRequestFolderName `shouldBe` "newName"


-- ** delete request node


    describe "delete request node" $ do
      it "returns 404 when request node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteRequestNodeHandler token 1 UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the request node doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId2 connection
          let nodeId = head requestNodes ^. requestNodeId
          try clientEnv (deleteRequestNodeHandler token requestCollectionId nodeId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a request node" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = head requestNodes ^. requestNodeId
          selectRequestNodeExists nodeId connection `shouldReturn` True
          _ <- try clientEnv (deleteRequestNodeHandler token requestCollectionId nodeId)
          selectRequestNodeExists nodeId connection `shouldReturn` False


-- ** duplicate node


    fdescribe "duplicate request node" $ do
      it "returns 404 when request node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let duplicateNode = mkDuplicateNode Nothing
          try clientEnv (duplicateNodeHandler token 1 UUID.nil duplicateNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the request node doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId2 connection
          let nodeId = head requestNodes ^. requestNodeId
          let duplicateNode = mkDuplicateNode Nothing
          try clientEnv (duplicateNodeHandler token requestCollectionId nodeId duplicateNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when try to duplicate a root folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = (getFirstFolder requestNodes & Maybe.fromJust) ^. requestNodeId
          let duplicateNode = mkDuplicateNode Nothing
          try clientEnv (duplicateNodeHandler token requestCollectionId nodeId duplicateNode) `shouldThrow`  errorsWithStatus HTTP.notFound404

      it "duplicate a file to the root" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = (getFirstFile requestNodes & Maybe.fromJust) ^. requestNodeId
          let duplicateNode = mkDuplicateNode Nothing
          try clientEnv (duplicateNodeHandler token requestCollectionId nodeId duplicateNode) `shouldReturn` ()
          origFile <- selectFakeRequestFile nodeId connection
          targetFile <- selectFakeRequestFile UUID.nil connection
          _fakeRequestFileName targetFile `shouldBe` (_fakeRequestFileName origFile) ++ " copy"

      it "duplicate a file to a folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = (getFirstFile requestNodes & Maybe.fromJust) ^. requestNodeId
          let parentId = (getFirstFolder requestNodes) <&> _requestNodeId
          let duplicateNode = mkDuplicateNode parentId
          try clientEnv (duplicateNodeHandler token requestCollectionId nodeId duplicateNode) `shouldReturn` ()
          origFile <- selectFakeRequestFile nodeId connection
          targetFile <- selectFakeRequestFile UUID.nil connection
          _fakeRequestFileName targetFile `shouldBe` (_fakeRequestFileName origFile) ++ " copy"
          _fakeRequestFileParentId targetFile `shouldBe` parentId


-- * util


  where
    updateRequestNode :: UpdateRequestNode
    updateRequestNode =
      UpdateRequestNode { _updateRequestNodeName = "newName" }

    mkDuplicateNode :: Maybe UUID -> DuplicateNode
    mkDuplicateNode mTargetId =
      DuplicateNode { _duplicateNodeNewId = UUID.nil
                    , _duplicateNodeTargetId = mTargetId
                    }
