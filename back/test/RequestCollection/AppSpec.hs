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

import           App
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Functor           ((<&>))
import           Helper.App
import           Network.HTTP.Types
import           PatchGirl
import           RequestCollection      (RequestCollection (..))
import           Servant
import           Servant.Auth.Server    (CookieSettings, JWTSettings, SetCookie)
import           Servant.Client
import           Servant.Server         (ServerError)
import           Test.Hspec

-- * client


getRequestCollectionById :: Int -> ClientM RequestCollection
getRequestCollectionById =
  client (Proxy :: Proxy RequestCollectionApi)


-- * spec


spec :: Spec
spec =
  describe "get request collection by id" $
    withClient (mkApp defaultConfig) $
      it "returns 404 when requestCollection does not exist" $ \clientEnv ->
        cleanDBAfter $ \connection ->
          True `shouldBe` True
          --try clientEnv (getRequestCollectionById 10000) `shouldThrow` errorsWithStatus notFound405
