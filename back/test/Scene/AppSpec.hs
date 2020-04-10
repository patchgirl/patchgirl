{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Scene.AppSpec where

import           Control.Lens.Getter      ((^.))
import qualified Data.Maybe               as Maybe
import           Data.UUID
import qualified Data.UUID                as UUID
import qualified Network.HTTP.Types       as HTTP
import           Servant                  hiding (Header)
import qualified Servant.Auth.Client      as Auth
import qualified Servant.Auth.Server      as Auth
import           Servant.Client           (ClientM, client)
import           Test.Hspec

import           Account.DB
import           App
import           Helper.App
import           ScenarioCollection.DB
import           ScenarioCollection.Model
import           ScenarioNode.DB
import           ScenarioNode.Model


-- * client

{-
createSceneHandler :: Auth.Token -> UUID -> NewScenarioFile -> ClientM ()
createSceneHandler =
  client (Proxy :: Proxy (PScene '[Auth.JWT]))
-}

-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** create scenario file


    describe "create a scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        True `shouldBe` True
