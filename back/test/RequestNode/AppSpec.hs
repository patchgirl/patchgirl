{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}


module RequestNode.AppSpec where

import           Control.Lens.Getter     ((^.))
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


updateRequestNode :: Auth.Token -> Int -> Int -> UpdateRequestNode -> ClientM NoContent
updateRequestNode =
  client (Proxy :: Proxy (PRequestNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** update request node


    describe "update request node" $ do
      it "returns 404 when request node doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          try clientEnv (updateRequestNode token 1 1 updateRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the request node doesnt belong to the account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId1, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          (accountId2, _) <- insertFakeAccount defaultNewFakeAccount2 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId2 connection
          let nodeId = (head requestNodes) ^. requestNodeId
          token <- signedUserToken accountId1
          try clientEnv (updateRequestNode token requestCollectionId nodeId updateRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a request folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let nodeId = (head requestNodes) ^. requestNodeId
          token <- signedUserToken accountId
          _ <- try clientEnv (updateRequestNode token requestCollectionId nodeId updateRequestFolder)
          FakeRequestFolder { _fakeRequestFolderName } <- selectFakeRequestFolder nodeId connection
          _fakeRequestFolderName `shouldBe` "newName"

  where
    updateRequestFile :: UpdateRequestNode
    updateRequestFile =
      UpdateRequestFile { _updateRequestNodeName = "newName"
                        , _updateRequestNodeHttpUrl = "http://newUrl.com"
                        , _updateRequestNodeHttpMethod = Post
                        , _updateRequestNodeHttpHeaders = [("newHeaderKey", "newHeaderValue")]
                        , _updateRequestNodeHttpBody = "newBody"
                        }

    updateRequestFolder :: UpdateRequestNode
    updateRequestFolder =
      UpdateRequestFolder { _updateRequestNodeName = "newName"
                          }
