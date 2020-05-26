{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module RequestCollection.AppSpec where

import           App
import           Helper.App
import qualified Network.HTTP.Types      as HTTP
import           RequestCollection.DB
import           RequestCollection.Model
import           Servant
import qualified Servant.Auth.Client     as Auth
import           Servant.Auth.Server     (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


getRequestCollectionById :: Auth.Token -> ClientM RequestCollection
getRequestCollectionById =
  client (Proxy :: Proxy (RequestCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $

    describe "get request collection by id" $ do
      it "returns notFound404 when requestCollection does not exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (getRequestCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty request collection if the account doesnt have a request collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          requestCollectionId <- insertFakeRequestCollection accountId connection
          requestCollection <- try clientEnv (getRequestCollectionById token)
          requestCollection `shouldBe` RequestCollection requestCollectionId []

      it "returns the account's request collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          expectedRequestCollection <- insertSampleRequestCollection accountId connection
          requestCollection <- try clientEnv (getRequestCollectionById token)
          requestCollection `shouldBe` expectedRequestCollection
