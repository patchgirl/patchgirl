{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module FrontConfig.App(getConfigApiHandler) where


import qualified Control.Exception           as Exception
import qualified Control.Monad.IO.Class      as IO
import qualified Control.Monad.Reader        as Reader
import qualified Data.ByteString.UTF8        as BSU
import qualified Data.CaseInsensitive        as CI
import           Data.Functor                ((<&>))
import qualified Data.Map.Strict             as Map
import qualified Network.HTTP.Client.Conduit as Http
import qualified Network.HTTP.Simple         as Http
import qualified Network.HTTP.Types          as Http

import           Http
import           Interpolator
import           PatchGirl
import           RequestComputation.Model


-- * handler


getConfigApiHandler :: (Reader.MonadReader Env m) => m FrontConfig
getConfigApiHandler = do
  Reader.ask <&> _envFrontConfig
