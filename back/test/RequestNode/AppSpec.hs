{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}


module RequestNode.AppSpec where

import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import           Servant.Client      (ClientM, client)
import           Test.Hspec

import           Account.DB
import           App
import           Helper.App
import           Http
import           RequestNode.Model


-- * client


updateRequestNode :: Auth.Token -> Int -> Int -> UpdateRequestNode -> ClientM NoContent
updateRequestNode =
  client (Proxy :: Proxy (PRequestNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** update request node


    describe "update request node" $
      it "returns 404 when request node doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          try clientEnv (updateRequestNode token 1 1 updateRequestFile) `shouldThrow` errorsWithStatus HTTP.notFound404

  where
    updateRequestFile :: UpdateRequestNode
    updateRequestFile =
      UpdateRequestFile { _updateRequestNodeName = "newName"
                        , _updateRequestNodeHttpUrl = "http://newUrl.com"
                        , _updateRequestNodeHttpMethod = Post
                        , _updateRequestNodeHttpHeaders = [("newHeaderKey", "newHeaderValue")]
                        , _updateRequestNodeHttpBody = "newBody"
                        }
