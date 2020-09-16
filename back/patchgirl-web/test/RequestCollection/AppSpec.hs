{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module RequestCollection.AppSpec where

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
import           PatchGirl.Web.RequestCollection.Model


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
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (getRequestCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty request collection if the account doesnt have a request collection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          requestCollectionId <- insertFakeRequestCollection accountId connection
          requestCollection <- try clientEnv (getRequestCollectionById token)
          requestCollection `shouldBe` RequestCollection requestCollectionId []

      it "returns the account's request collection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          expectedRequestCollection <- insertSampleRequestCollection accountId connection
          requestCollection <- try clientEnv (getRequestCollectionById token)
          requestCollection `shouldBe` expectedRequestCollection
