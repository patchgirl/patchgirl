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

module RequestCollection.AppSpec where

import           Account.DB
import           App
import qualified Database.PostgreSQL.Simple as PG
import           Helper.App
import qualified Network.HTTP.Types         as HTTP
import           RequestCollection.DB
import qualified RequestCollection.DB2      as DB2
import           RequestCollection.Model
import           Servant
import qualified Servant.Auth.Client        as Auth
import           Servant.Auth.Server        (JWT)
import           Servant.Client
import           Test.Hspec

-- * client


getRequestCollectionById :: Auth.Token -> Int -> ClientM RequestCollection
getRequestCollectionById2 :: Auth.Token -> Int -> ClientM RequestCollection
getRequestCollectionById :<|> getRequestCollectionById2 =
  client (Proxy :: Proxy (PRequestCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- * spec1


    describe "get request collection by id" $ do
      it "returns notFound404 when requestCollection does not exist" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          token <- signedUserToken 1
          try clientEnv (getRequestCollectionById token 10000) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns unauthorized404 when requestCollection does not belong to user" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          FakeRequestCollection { _fakeRequestCollectionId } <- insertFakeRequestCollection accountId connection
          token <- signedUserToken (accountId + 1)
          try clientEnv (getRequestCollectionById token _fakeRequestCollectionId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns the request collection when request collection exists and belongs to user" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          FakeRequestCollection { _fakeRequestCollectionId } <- insertFakeRequestCollection accountId connection
          _ <- insertRequestNodes connection
          let fakeRequestCollectionToRequestNode1 =
                FakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = _fakeRequestCollectionId
                                                   , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = 1
                                                   }
          let fakeRequestCollectionToRequestNode2 =
                FakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = _fakeRequestCollectionId
                                                   , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = 2
                                                   }

          _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode1 connection
          _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode2 connection
          token <- signedUserToken accountId
          requestCollection <- try clientEnv (getRequestCollectionById token _fakeRequestCollectionId)
          requestCollection `shouldBe` RequestCollection _fakeRequestCollectionId []


-- * spec2


    describe "get request collection by id" $ do
      it "returns notFound404 when requestCollection does not exist" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          token <- signedUserToken 1
          try clientEnv (getRequestCollectionById2 token 10000) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns unauthorized404 when requestCollection does not belong to user" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          requestCollectionId <- DB2.insertFakeRequestCollection accountId connection
          token <- signedUserToken (accountId + 1)
          try clientEnv (getRequestCollectionById2 token requestCollectionId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns the request collection when request collection exists and belongs to user" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          expectedRequestCollection@(RequestCollection requestCollectionId _) <- DB2.insertSampleRequestCollection accountId connection
          token <- signedUserToken accountId
          requestCollection <- try clientEnv (getRequestCollectionById2 token requestCollectionId)
          requestCollection `shouldBe` expectedRequestCollection

  where

    insertRequestNodes :: PG.Connection -> IO ()
    insertRequestNodes connection = do
      _ <- insertFakeRequestFolder n1 connection
      _ <- insertFakeRequestFolder n2 connection
      _ <- insertFakeRequestFolder n3 connection
      _ <- insertFakeRequestFile n4 connection
      _ <- insertFakeRequestFile n5 connection
      _ <- insertFakeRequestFile n6 connection
      return ()

    -- level 1

    n1 = FakeRequestFolder { _fakeRequestFolderId       = 1
                           , _fakeRequestFolderParentId = Nothing
                           , _fakeRequestName           = "1/"
                           }

    n2 = FakeRequestFolder { _fakeRequestFolderId       = 2
                           , _fakeRequestFolderParentId = Nothing
                           , _fakeRequestName           = "2/"
                           }
    -- level 2

    n3 = FakeRequestFolder { _fakeRequestFolderId       = 3
                           , _fakeRequestFolderParentId = Just 1
                           , _fakeRequestName           = "3/"
                           }

    n4 = FakeRequestFile { _fakeRequestFileId = 4
                         , _fakeRequestFileParentId = Just 1
                         , _fakeRequestFileName       = "4"
                         , _fakeRequestFileHttpUrl    = "http://4.com"
                         , _fakeRequestFileHttpMethod = "Get"
                         , _fakeRequestFileHttpBody   = ""
                         }

    -- level 3

    n5 = FakeRequestFile { _fakeRequestFileId = 5
                         , _fakeRequestFileParentId = Just 3
                         , _fakeRequestFileName       = "5"
                         , _fakeRequestFileHttpUrl    = "http://5.com"
                         , _fakeRequestFileHttpMethod = "Get"
                         , _fakeRequestFileHttpBody   = ""
                         }

    n6 = FakeRequestFile { _fakeRequestFileId = 6
                         , _fakeRequestFileParentId = Just 3
                         , _fakeRequestFileName       = "6"
                         , _fakeRequestFileHttpUrl    = "http://6.com"
                         , _fakeRequestFileHttpMethod = "Get"
                         , _fakeRequestFileHttpBody   = ""
                         }
