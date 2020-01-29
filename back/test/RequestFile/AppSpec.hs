module RequestFile.AppSpec where

import           Test.Hspec

{-
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}



import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import           Servant.Client      (ClientM, client)

import           Account.DB
import           App
import           Helper.App
import           Http
import           RequestNode.Model


-- * client


createRequestFile :: Auth.Token -> Int -> NewRequestFile -> ClientM Int
createRequestFile =
  client (Proxy :: Proxy (PRequestFileApi '[Auth.JWT]))


-- * spec

-}

spec :: Spec
spec =
    describe "" $
      it ""
        pending
