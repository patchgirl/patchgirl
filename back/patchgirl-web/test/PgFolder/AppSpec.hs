{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module PgFolder.AppSpec where

import           Data.Function                    ((&))
import qualified Data.Maybe                       as Maybe
import qualified Network.HTTP.Types               as HTTP
import           Servant
import qualified Servant.Auth.Client              as Auth
import qualified Servant.Auth.Server              as Auth
import           Servant.Client                   (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Id
import           PatchGirl.Web.PgCollection.Model
import           PatchGirl.Web.PgNode.Model
import           PatchGirl.Web.Server


-- * client


createPgFolder :: Auth.Token -> Id PgCollection -> NewPgFolder -> ClientM ()
createRootPgFolder :: Auth.Token -> Id PgCollection -> NewRootPgFolder -> ClientM ()
createPgFolder :<|> createRootPgFolder =
  client (Proxy :: Proxy (PgFolderApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create pg folder


    describe "create a pg folder" $ do
      it "returns 404 when pg collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newPgFile = mkNewPgFolder nilId nilId
          try clientEnv (createPgFolder token nilId newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when pg node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId _ <- insertSamplePgCollection accountId connection
          let newPgFile = mkNewPgFolder nilId nilId
          try clientEnv (createPgFolder token pgCollectionId newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when pg node parent exist but isn't a pg folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let fileId = Maybe.fromJust (getFirstPgFile pgNodes) & _pgNodeId
          let newPgFile = mkNewPgFolder nilId fileId
          try clientEnv (createPgFolder token pgCollectionId newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the pg folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let folderId = Maybe.fromJust (getFirstPgFolder pgNodes) & _pgNodeId
          let newPgFolder = mkNewPgFolder nilId folderId
          _ <- try clientEnv (createPgFolder token pgCollectionId newPgFolder)
          fakePgFolder <- selectFakePgFolder nilId connection
          fakePgFolder `shouldBe`  FakePgFolder { _fakePgFolderParentId   = Just folderId
                                                , _fakePgFolderName       = "whatever"
                                                }
-- ** create root pg folder


    describe "create a root pg folder" $ do
      it "returns 404 when pg collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRootPgFolder = mkNewRootPgFolder nilId
          try clientEnv (createRootPgFolder token nilId newRootPgFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the pg folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          pgCollectionId <- insertFakePgCollection accountId connection
          let newRootPgFolder = mkNewRootPgFolder nilId
          _ <- try clientEnv (createRootPgFolder token pgCollectionId newRootPgFolder)
          fakePgFolder <- selectFakePgFolder nilId connection
          fakePgFolder `shouldBe`  FakePgFolder { _fakePgFolderParentId = Nothing
                                                , _fakePgFolderName       = "name"
                                                }

  where
    mkNewPgFolder :: Id Postgres -> Id Postgres -> NewPgFolder
    mkNewPgFolder id parentId =
      NewPgFolder { _newPgFolderId           = id
                  , _newPgFolderParentNodeId = parentId
                  , _newPgFolderName = "whatever"
                  }

    mkNewRootPgFolder :: Id Postgres -> NewRootPgFolder
    mkNewRootPgFolder id =
      NewRootPgFolder { _newRootPgFolderId = id
                      , _newRootPgFolderName = "name"
                      }
