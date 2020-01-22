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
import           Helper.App
import qualified Network.HTTP.Types    as HTTP
import           RequestCollection.App (RequestCollection (..))
import           RequestCollection.DB
import           Servant
import qualified Servant.Auth.Client   as Auth
import           Servant.Auth.Server   (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


getRequestCollectionById :: Auth.Token -> Int -> ClientM RequestCollection
getRequestCollectionById =
  client (Proxy :: Proxy (PRequestCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $
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
          token <- signedUserToken accountId
          requestCollection <- try clientEnv (getRequestCollectionById token _fakeRequestCollectionId)
          requestCollection `shouldBe` RequestCollection _fakeRequestCollectionId []
