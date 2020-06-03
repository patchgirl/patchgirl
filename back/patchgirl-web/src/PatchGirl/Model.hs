{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PatchGirl.Model where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader

import           Servant                hiding (BadPassword, NoSuchUser)

import           Env


newtype AppM a =
  AppM { unAppM :: Except.ExceptT ServerError (Reader.ReaderT Env IO) a }
  deriving ( Except.MonadError ServerError
           , Reader.MonadReader Env
           , Functor
           , Applicative
           , Monad
           , IO.MonadIO
           )

appMToHandler
  :: Env
  -> AppM a
  -> Handler a
appMToHandler env r = do
  eitherErrorOrResult <- IO.liftIO $ flip Reader.runReaderT env . Except.runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left error   -> throwError error
    Right result -> return result
