module PatchGirl.Web.PatchGirl
  ( module Env
  , logError
  ) where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader

import           PatchGirl.Web.Internal.Env as Env

logError
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => String
  -> m ()
logError msg = do
  env <- Reader.ask
  IO.liftIO $ _envLog env msg
