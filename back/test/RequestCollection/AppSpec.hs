{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module RequestCollection.AppSpec where

import           Account.DB
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
  client (Proxy :: Proxy (PRequestCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $

    describe "get request collection by id" $ do
      it "returns notFound404 when requestCollection does not exist" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          token <- signedUserToken 1
          try clientEnv (getRequestCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns the account's request collection" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          expectedRequestCollection <- insertSampleRequestCollection accountId connection
          token <- signedUserToken accountId
          requestCollection <- try clientEnv (getRequestCollectionById token)
          requestCollection `shouldBe` expectedRequestCollection
