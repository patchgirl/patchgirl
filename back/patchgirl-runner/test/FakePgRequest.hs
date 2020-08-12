module FakePgRequest (defaultPgComputationInput) where

import qualified Control.Exception      as Exception
import           Control.Lens.Operators ((.~))
import qualified Control.Monad          as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.UTF8   as BSU
import           Data.Functor           ((<&>))
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Network.HTTP.Client    as HTTP
import qualified Network.HTTP.Types     as HTTP
import           Servant

import           Env
import           Helper.App
import           Interpolator
import           PatchGirl.Client
import           PgSqlComputation.Model
import           Server


-- * request computation input util


defaultPgComputationInput :: PgComputationInput
defaultPgComputationInput =
  PgComputationInput { _pgComputationInputSql             = []
                     , _pgComputationInputPgConnection    =
                       TemplatedPgConnection { _templatedPgConnectionHost = []
                                             , _templatedPgConnectionPort         = []
                                             , _templatedPgConnectionUser         = []
                                             , _templatedPgConnectionPassword     = []
                                             , _templatedPgConnectionDbName       = []
                                             }
                     }
