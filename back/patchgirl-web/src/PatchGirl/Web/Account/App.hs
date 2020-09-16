{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module PatchGirl.Web.Account.App where

import qualified Control.Monad                    as Monad
import qualified Control.Monad.Reader as Reader
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.Trans              (liftIO)
import qualified Database.PostgreSQL.Simple       as PG
import           Data.Functor               ((<&>))

import           PatchGirl.Web.DB
import           PatchGirl.Web.PatchGirl


-- * handler


resetVisitorAccountHandler
  :: ( MonadReader Env m
     , MonadIO m
     )
  => m ()
resetVisitorAccountHandler = do
  connection <- getDBConnection
  resetVisitorData <- Reader.ask <&> _envResetVisitorData
  liftIO $ Monad.void $ PG.execute_ connection resetVisitorData
