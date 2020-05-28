{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module FrontConfig.App(getConfigApiHandler) where


import qualified Control.Monad.Reader as Reader
import           Data.Functor         ((<&>))

import           PatchGirl


-- * handler


getConfigApiHandler :: (Reader.MonadReader Env m) => m FrontConfig
getConfigApiHandler = do
  Reader.ask <&> _envFrontConfig
