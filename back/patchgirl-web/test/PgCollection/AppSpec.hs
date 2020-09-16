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
import           PatchGirl.Web.Server
import           PatchGirl.Web.Api
import           PatchGirl.Web.PgCollection.Model


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
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (getPgCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty pg collection if the account doesnt have a pg collection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          pgCollectionId <- insertFakePgCollection accountId connection
          pgCollection <- try clientEnv (getPgCollectionById token)
          pgCollection `shouldBe` PgCollection pgCollectionId []

      it "returns the account's pg collection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          expectedPgCollection <- insertSamplePgCollection accountId connection
          pgCollection <- try clientEnv (getPgCollectionById token)
          pgCollection `shouldBe` expectedPgCollection
