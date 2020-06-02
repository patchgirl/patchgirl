{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module RunnerConfig.App(getConfigApiHandler) where


import qualified Control.Monad.Reader as Reader
import           Data.Functor         ((<&>))

import           PatchGirl


-- * handler


getConfigApiHandler :: (Reader.MonadReader Env m) => m RunnerConfig
getConfigApiHandler = do
  Reader.ask <&> _envRunnerConfig
