{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module RequestFile.AppSpec where

import           Data.Coerce                           (coerce)
import           Data.Function                         ((&))
import qualified Data.Maybe                            as Maybe
import qualified Network.HTTP.Types                    as HTTP
import           Servant                               hiding (Header)
import qualified Servant.Auth.Client                   as Auth
import qualified Servant.Auth.Server                   as Auth
import           Servant.Client                        (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Http
import           PatchGirl.Web.Id
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.Server


-- * client


createRequestFileHandler :: Auth.Token -> Int -> NewRequestFile -> ClientM ()
createRootRequestFileHandler :: Auth.Token -> Int -> NewRootRequestFile -> ClientM ()
updateRequestFileHandler :: Auth.Token -> Int -> Id Request -> UpdateRequestFile -> ClientM ()
createRequestFileHandler
  :<|> createRootRequestFileHandler
  :<|> updateRequestFileHandler =
  client (Proxy :: Proxy (RequestFileApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create request file


    describe "create a request file" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRequestFile = mkNewRequestFile nilId nilId
          try clientEnv (createRequestFileHandler token 1 newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when request node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId _ <- insertSampleRequestCollection accountId connection
          let newRequestFile = mkNewRequestFile nilId nilId
          try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 500 when request node parent exist but isn't a request folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let fileId = coerce $ Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let newRequestFile = mkNewRequestFile nilId fileId
          try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let folderId = coerce $ Maybe.fromJust (getFirstFolder requestNodes) & _requestNodeId
          let newRequestFile = mkNewRequestFile nilId folderId
          _ <- try clientEnv (createRequestFileHandler token requestCollectionId newRequestFile)
          fakeRequestFile <- selectFakeRequestFile nilId connection
          fakeRequestFile `shouldBe`  FakeRequestFile { _fakeRequestFileParentId   = Just folderId
                                                      , _fakeRequestFileName       = "test"
                                                      , _fakeRequestFileHttpUrl    = "http://foo.com"
                                                      , _fakeRequestFileHttpMethod = Get
                                                      , _fakeRequestFileHttpBody   = "body"
                                                      , _fakeRequestFileHttpHeaders = HttpHeaders [ Header ("key", "value") ]
                                                      }

-- ** create root request file


    describe "create a root request file" $ do
      it "returns 404 when request collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRootRequestFile = mkNewRootRequestFile nilId
          try clientEnv (createRootRequestFileHandler token 1 newRootRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the request file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          requestCollectionId <- insertFakeRequestCollection accountId connection
          let newRootRequestFile = mkNewRootRequestFile nilId
          _ <- try clientEnv (createRootRequestFileHandler token requestCollectionId newRootRequestFile)
          fakeRequestFile <- selectFakeRequestFile nilId connection
          fakeRequestFile `shouldBe`  FakeRequestFile { _fakeRequestFileParentId   = Nothing
                                                      , _fakeRequestFileName       = "test"
                                                      , _fakeRequestFileHttpUrl    = "http://foo.com"
                                                      , _fakeRequestFileHttpMethod = Get
                                                      , _fakeRequestFileHttpBody   = "body"
                                                      , _fakeRequestFileHttpHeaders = HttpHeaders [ Header ("key", "value") ]
                                                      }


-- ** update request file


    describe "update a request file" $ do
      it "returns 404 when request file doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let updateRequestFile = mkUpdateRequestFile
          try clientEnv (updateRequestFileHandler token 1 nilId updateRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "update the request file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          RequestCollection requestCollectionId requestNodes <- insertSampleRequestCollection accountId connection
          let RequestFile {..} = Maybe.fromJust $ getFirstFile requestNodes
          _ <- try clientEnv (updateRequestFileHandler token requestCollectionId (coerce _requestNodeId) mkUpdateRequestFile)
          FakeRequestFile{..} <- selectFakeRequestFile (coerce _requestNodeId) connection
          _fakeRequestFileName `shouldBe` "new name"
          _fakeRequestFileHttpUrl `shouldBe` "https://newUrl.com"
          _fakeRequestFileHttpMethod `shouldBe` Patch
          _fakeRequestFileHttpBody `shouldBe` "new body"
          _fakeRequestFileHttpHeaders `shouldBe` HttpHeaders [ Header ("newHeader1", "newValue1")
                                                             , Header ("newHeader2", "newValue2")
                                                             ]


-- * util


  where
    mkNewRequestFile :: Id Request -> Id Request -> NewRequestFile
    mkNewRequestFile id parentId =
      NewRequestFile { _newRequestFileId           = id
                     , _newRequestFileParentNodeId = parentId
                     , _newRequestFileName         = "test"
                     , _newRequestFileHttpUrl = "http://foo.com"
                     , _newRequestFileMethod  = Get
                     , _newRequestFileHeaders = [ HttpHeader ("key", "value") ]
                     , _newRequestFileBody    = "body"
                     }

    mkNewRootRequestFile :: Id Request -> NewRootRequestFile
    mkNewRootRequestFile id =
      NewRootRequestFile { _newRootRequestFileId = id
                         , _newRootRequestFileName = "test"
                         , _newRootRequestFileHttpUrl = "http://foo.com"
                         , _newRootRequestFileMethod = Get
                         , _newRootRequestFileHeaders = [ HttpHeader ("key", "value") ]
                         , _newRootRequestFileBody = "body"
                         }

    mkUpdateRequestFile :: UpdateRequestFile
    mkUpdateRequestFile =
      UpdateRequestFile { _updateRequestFileName        = "new name"
                        , _updateRequestFileHttpUrl     = "https://newUrl.com"
                        , _updateRequestFileHttpMethod  = Patch
                        , _updateRequestFileHttpHeaders = [ HttpHeader ("newHeader1", "newValue1")
                                                          , HttpHeader ("newHeader2", "newValue2")
                                                          ]
                        , _updateRequestFileHttpBody    = "new body"
                        }
