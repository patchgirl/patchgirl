
module Test where

import           Control.Monad.Except     (MonadError)
import           Control.Monad.Reader     (MonadReader)
import           Env
import           PatchGirl
import           Servant                  (err404, err500, throwError)
import           Servant.API.ContentTypes (NoContent (..))
import           Servant.Server           (ServerError)

-- * Handler


deleteNoContentHandler
  :: ( MonadReader Env m
     )
  => m NoContent
deleteNoContentHandler =
  return NoContent

getNotFoundHandler
  :: ( MonadError ServerError m
     )
  => m ()
getNotFoundHandler =
  throwError err404

getInternalServerErrorHandler
  :: ( MonadError ServerError m
     )
  => m ()
getInternalServerErrorHandler =
  throwError err500
