module Test where

import           Servant (Handler, throwError, err404, err500)
import Servant.API.ContentTypes (NoContent(..))

-- * Handler

deleteNoContentHandler :: Handler NoContent
deleteNoContentHandler =
  return NoContent

getNotFoundHandler :: Handler ()
getNotFoundHandler =
  throwError err404

getInternalServerErrorHandler :: Handler ()
getInternalServerErrorHandler =
  throwError err500
