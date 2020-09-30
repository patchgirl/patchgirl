{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}



module RequestNode.AppSpec where

import           Data.Function                         ((&))
import           Data.UUID
import qualified Data.UUID                             as UUID
import qualified Network.HTTP.Types                    as HTTP
import           Servant
import qualified Servant.Auth.Client                   as Auth
import qualified Servant.Auth.Server                   as Auth
import           Servant.Client                        (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.Server


-- * client


updateRequestNodeHandler :: Auth.Token -> Int -> UUID -> UpdateRequestNode -> ClientM ()
deleteRequestNodeHandler :: Auth.Token -> Int -> UUID -> ClientM ()
updateRequestNodeHandler :<|> deleteRequestNodeHandler =
  client (Proxy :: Proxy (RequestNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
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
          let nodeId = head requestNodes & _requestNodeId
          try clientEnv (updateRequestNodeHandler token requestCollectionId nodeId updateRequestNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = head requestNodes & _requestNodeId
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
          let nodeId = head requestNodes & _requestNodeId
          try clientEnv (deleteRequestNodeHandler token requestCollectionId nodeId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a request node" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = head requestNodes & _requestNodeId
          selectRequestNodeExists nodeId connection `shouldReturn` True
          _ <- try clientEnv (deleteRequestNodeHandler token requestCollectionId nodeId)
          selectRequestNodeExists nodeId connection `shouldReturn` False


-- * util


  where
    updateRequestNode :: UpdateRequestNode
    updateRequestNode =
      UpdateRequestNode { _updateRequestNodeName = "newName" }
