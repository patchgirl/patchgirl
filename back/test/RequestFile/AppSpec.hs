{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module RequestFile.AppSpec where

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
import           Http
import           RequestCollection.DB
import           RequestCollection.Model
import           RequestNode.DB
import           RequestNode.Model


-- * client


createRequestFileHandler :: Auth.Token -> Int -> NewRequestFile -> ClientM ()
createRootRequestFileHandler :: Auth.Token -> Int -> NewRootRequestFile -> ClientM ()
createRequestFileHandler :<|> createRootRequestFileHandler =
  client (Proxy :: Proxy (PRequestFileApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** create request file


    describe "create a request file" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFile UUID.nil UUID.nil
          try clientEnv (createRequestFileHandler token 1 newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when request node parent doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          RequestCollection requestCollectionId _ <- insertSampleRequestCollection accountId connection
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFile UUID.nil UUID.nil
          try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 500 when request node parent exist but isn't a request folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let fileId = Maybe.fromJust (getFirstFile requestNodes) ^. requestNodeId
          token <- signedUserToken accountId
          let newRequestFile = mkNewRequestFile UUID.nil fileId
          try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request file" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let folderId = Maybe.fromJust (getFirstFolder requestNodes) ^. requestNodeId
          let newRequestFile = mkNewRequestFile UUID.nil folderId
          _ <- try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile)
          fakeRequestFile <- selectFakeRequestFile UUID.nil connection
          fakeRequestFile `shouldBe`  FakeRequestFile { _fakeRequestFileParentId   = Just folderId
                                                      , _fakeRequestFileName       = "new request"
                                                      , _fakeRequestFileHttpUrl    = ""
                                                      , _fakeRequestFileHttpMethod = Get
                                                      , _fakeRequestFileHttpBody   = ""
                                                      }

-- ** create root request file


    describe "create a root request file" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          let newRootRequestFile = mkNewRootRequestFile UUID.nil
          try clientEnv (createRootRequestFileHandler token 1 newRootRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request file" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          requestCollectionId <- insertFakeRequestCollection accountId connection
          let newRootRequestFile = mkNewRootRequestFile UUID.nil
          _ <- try clientEnv (createRootRequestFileHandler token requestCollectionId newRootRequestFile)
          fakeRequestFile <- selectFakeRequestFile UUID.nil connection
          fakeRequestFile `shouldBe`  FakeRequestFile { _fakeRequestFileParentId   = Nothing
                                                      , _fakeRequestFileName       = "new request"
                                                      , _fakeRequestFileHttpUrl    = ""
                                                      , _fakeRequestFileHttpMethod = Get
                                                      , _fakeRequestFileHttpBody   = ""
                                                      }

  where
    mkNewRequestFile :: UUID -> UUID -> NewRequestFile
    mkNewRequestFile id parentId =
      NewRequestFile { _newRequestFileId           = id
                     , _newRequestFileParentNodeId = parentId
                     }

    mkNewRootRequestFile :: UUID -> NewRootRequestFile
    mkNewRootRequestFile id =
      NewRootRequestFile { _newRootRequestFileId = id }
