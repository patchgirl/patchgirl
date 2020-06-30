module Health.App where

import qualified Control.Monad.IO.Class as IO

healthHandler :: (IO.MonadIO m) => m ()
healthHandler =
  return ()
