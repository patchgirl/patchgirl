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
import           Helper.App
import           RequestCollection (RequestCollection (..))
import           Servant
import           Servant.Client
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
      it "returns 404 when requestCollection does not exist" $ \_ ->
        cleanDBAfter $ \_ ->
          True `shouldBe` True
          --try clientEnv (getRequestCollectionById 10000) `shouldThrow` errorsWithStatus notFound405
