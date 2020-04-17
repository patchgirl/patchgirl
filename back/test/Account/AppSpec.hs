{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module Account.AppSpec where

import           App
import           Helper.App
import           Servant
import           Servant.Client (ClientM, client)
import           Test.Hspec


-- * client


resetVisitorAccount :: ClientM ()
resetVisitorAccount =
  client (Proxy :: Proxy AccountApi)


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** reset visitor account


    describe "reset visitor account" $
      it "should returns 200" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          res <- try clientEnv resetVisitorAccount
          res `shouldBe` ()
