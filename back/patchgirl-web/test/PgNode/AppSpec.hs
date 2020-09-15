{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}



module PgNode.AppSpec where

import           Control.Lens.Getter ((^.))
import           Data.UUID
import qualified Data.UUID           as UUID
import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import           Servant.Client      (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Server
import           PatchGirl.Api
import           PgCollection.Model
import           PgNode.Model


-- * client


updatePgNodeHandler :: Auth.Token -> UUID -> UUID -> UpdatePgNode -> ClientM ()
deletePgNodeHandler :: Auth.Token -> UUID -> UUID -> ClientM ()
updatePgNodeHandler :<|> deletePgNodeHandler =
  client (Proxy :: Proxy (PgNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** update pg node


    describe "update pg node" $ do
      it "returns 404 when pg node doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (updatePgNodeHandler token UUID.nil UUID.nil updatePgNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the pg node doesnt belong to the account" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId2 connection
          let nodeId = head pgNodes ^. pgNodeId
          try clientEnv (updatePgNodeHandler token pgCollectionId nodeId updatePgNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a pg folder" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let nodeId = head pgNodes ^. pgNodeId
          _ <- try clientEnv (updatePgNodeHandler token pgCollectionId nodeId updatePgNode)
          FakePgFolder { _fakePgFolderName } <- selectFakePgFolder nodeId connection
          _fakePgFolderName `shouldBe` "newName"


-- ** delete pg node


    describe "delete pg node" $ do
      it "returns 404 when pg node doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (deletePgNodeHandler token UUID.nil UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the pg node doesnt belong to the account" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId2 connection
          let nodeId = head pgNodes ^. pgNodeId
          try clientEnv (deletePgNodeHandler token pgCollectionId nodeId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a pg node" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let nodeId = head pgNodes ^. pgNodeId
          selectPgNodeExists nodeId connection `shouldReturn` True
          _ <- try clientEnv (deletePgNodeHandler token pgCollectionId nodeId)
          selectPgNodeExists nodeId connection `shouldReturn` False

  where
    updatePgNode :: UpdatePgNode
    updatePgNode =
      UpdatePgNode { _updatePgNodeName = "newName" }
