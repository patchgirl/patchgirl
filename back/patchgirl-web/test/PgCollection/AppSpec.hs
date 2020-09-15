{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module PgCollection.AppSpec where

import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import           Servant.Auth.Server (JWT)
import           Servant.Client
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Server
import           PatchGirl.Api
import           PgCollection.Model


-- * client


getPgCollectionById :: Auth.Token -> ClientM PgCollection
getPgCollectionById =
  client (Proxy :: Proxy (PgCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $

    describe "get pg collection by id" $ do
      it "returns notFound404 when pgCollection does not exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (getPgCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty pg collection if the account doesnt have a pg collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          pgCollectionId <- insertFakePgCollection accountId connection
          pgCollection <- try clientEnv (getPgCollectionById token)
          pgCollection `shouldBe` PgCollection pgCollectionId []

      it "returns the account's pg collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          expectedPgCollection <- insertSamplePgCollection accountId connection
          pgCollection <- try clientEnv (getPgCollectionById token)
          pgCollection `shouldBe` expectedPgCollection
