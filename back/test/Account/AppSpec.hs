{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Account.AppSpec where

import           Account.App
import           Account.DB
import           Account.Model
import           App
import           Control.Lens       ((^.))
import           Data.Maybe         (isJust, isNothing)
import           Helper.App
import           Model
import           Network.HTTP.Types (badRequest400)
import           Servant
import           Servant.Client     (ClientM, client)
import           Session.Model
import           Test.Hspec


-- * client


resetVisitorAccount :: ClientM ()
resetVisitorAccount =
  client (Proxy :: Proxy AccountApi)


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** reset visitor account


    describe "reset visitor account" $
      it "should returns 200" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          res <- try clientEnv resetVisitorAccount
          res `shouldBe` ()
