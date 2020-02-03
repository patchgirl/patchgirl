{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module RequestFolder.AppSpec where

import           Control.Lens.Getter     ((^.))
import qualified Data.Maybe              as Maybe
import           Data.UUID
import qualified Data.UUID               as UUID
import qualified Network.HTTP.Types      as HTTP
import           Servant
import qualified Servant.Auth.Client     as Auth
import qualified Servant.Auth.Server     as Auth
import           Servant.Client          (ClientM, client)
import           Test.Hspec

import           Account.DB
import           App
import           Helper.App
import           RequestCollection.DB
import           RequestCollection.Model
import           RequestNode.DB
import           RequestNode.Model


-- * client


createRequestFolder :: Auth.Token -> Int -> NewRequestFolder -> ClientM ()
createRequestFolder =
  client (Proxy :: Proxy (PRequestFolderApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** create request folder


    describe "create a request folder" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFolder UUID.nil UUID.nil
          try clientEnv (createRequestFolder token 1 newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when request node parent doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          RequestCollection requestCollectionId _ <- insertSampleRequestCollection accountId connection
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFolder UUID.nil UUID.nil
          try clientEnv (createRequestFolder token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 500 when request node parent exist but isn't a request folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let fileId = Maybe.fromJust (getFirstFile requestNodes) ^. requestNodeId
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFolder UUID.nil fileId
          try clientEnv (createRequestFolder token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let folderId = Maybe.fromJust (getFirstFolder requestNodes) ^. requestNodeId
          let newRequestFolder = mkNewRequestFolder UUID.nil folderId
          _ <- try clientEnv (createRequestFolder token requestCollectionId newRequestFolder)
          fakeRequestFolder <- selectFakeRequestFolder UUID.nil connection
          fakeRequestFolder `shouldBe`  FakeRequestFolder { _fakeRequestFolderParentId   = Just folderId
                                                          , _fakeRequestFolderName       = "whatever"
                                                          }

  where
    mkNewRequestFolder :: UUID -> UUID -> NewRequestFolder
    mkNewRequestFolder id parentId =
      NewRequestFolder { _newRequestFolderId           = id
                       , _newRequestFolderParentNodeId = parentId
                       , _newRequestFolderName = "whatever"
                       }
