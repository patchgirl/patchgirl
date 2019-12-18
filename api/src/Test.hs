{-# LANGUAGE FlexibleContexts #-}

module Test where

import           Control.Monad.Except     (MonadError)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (MonadReader)
import           Servant                  (err404, err500, throwError)
import           Servant.API.ContentTypes (NoContent (..))
import           Servant.Server           (ServerError)

-- * Handler

deleteNoContentHandler
  :: ( MonadReader String m
     , MonadIO m
     , MonadError ServerError m
     )
  => m NoContent
deleteNoContentHandler =
  return NoContent

getNotFoundHandler
  :: ( MonadReader String m
     , MonadIO m
     , MonadError ServerError m
     )
  => m ()
getNotFoundHandler =
  throwError err404

getInternalServerErrorHandler
  :: ( MonadReader String m
     , MonadIO m
     , MonadError ServerError m
     )
  => m ()
getInternalServerErrorHandler =
  throwError err500
