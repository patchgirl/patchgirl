{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module RequestFolder.AppSpec where

import           Data.Coerce                           (coerce)
import           Data.Function                         ((&))
import qualified Data.Maybe                            as Maybe
import qualified Network.HTTP.Types                    as HTTP
import           Servant
import qualified Servant.Auth.Client                   as Auth
import qualified Servant.Auth.Server                   as Auth
import           Servant.Client                        (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Id
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.Server


-- * client


createRequestFolder :: Auth.Token -> Int -> NewRequestFolder -> ClientM ()
createRootRequestFolder :: Auth.Token -> Int -> NewRootRequestFolder -> ClientM ()
createRequestFolder :<|> createRootRequestFolder =
  client (Proxy :: Proxy (RequestFolderApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create request folder


    describe "create a request folder" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRequestFile = mkNewRequestFolder nilId nilId
          try clientEnv (createRequestFolder token 1 newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when request node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId _ <- insertSampleRequestCollection accountId connection
          let newRequestFile = mkNewRequestFolder nilId nilId
          try clientEnv (createRequestFolder token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 500 when request node parent exist but isn't a request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let fileId = coerce $ Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let newRequestFile = mkNewRequestFolder nilId fileId
          try clientEnv (createRequestFolder token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let folderId = coerce $ Maybe.fromJust (getFirstFolder requestNodes) & _requestNodeId
          let newRequestFolder = mkNewRequestFolder nilId folderId
          _ <- try clientEnv (createRequestFolder token requestCollectionId newRequestFolder)
          fakeRequestFolder <- selectFakeRequestFolder nilId connection
          fakeRequestFolder `shouldBe`  FakeRequestFolder { _fakeRequestFolderParentId   = Just folderId
                                                          , _fakeRequestFolderName       = "whatever"
                                                          }
-- ** create root request folder


    describe "create a root request folder" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRootRequestFolder = mkNewRootRequestFolder nilId
          try clientEnv (createRootRequestFolder token 1 newRootRequestFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          requestCollectionId <- insertFakeRequestCollection accountId connection
          let newRootRequestFolder = mkNewRootRequestFolder nilId
          _ <- try clientEnv (createRootRequestFolder token requestCollectionId newRootRequestFolder)
          fakeRequestFolder <- selectFakeRequestFolder nilId connection
          fakeRequestFolder `shouldBe`  FakeRequestFolder { _fakeRequestFolderParentId = Nothing
                                                          , _fakeRequestFolderName       = "test"
                                                          }

  where
    mkNewRequestFolder :: Id Request -> Id Request -> NewRequestFolder
    mkNewRequestFolder id parentId =
      NewRequestFolder { _newRequestFolderId           = id
                       , _newRequestFolderParentNodeId = parentId
                       , _newRequestFolderName = "whatever"
                       }

    mkNewRootRequestFolder :: Id Request -> NewRootRequestFolder
    mkNewRootRequestFolder id =
      NewRootRequestFolder { _newRootRequestFolderId = id
                           , _newRootRequestFolderName = "test"
                           }
