module FakeHttpRequest ( throwException
                       , withHttpMock
                       , withExceptionHttpMock
                       , defaultRequest
                       , defaultRequestComputationInput
                       ) where

import           App
import qualified Control.Exception        as Exception
import           Control.Lens.Operators   ((.~))
import qualified Control.Monad.IO.Class   as IO
import qualified Data.ByteString.UTF8     as BSU
import           Data.Functor             ((<&>))
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Env
import           Helper.App
import qualified Http
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import           RequestComputation.Model
import           Servant
import           System.IO.Unsafe         as IO


throwException :: HTTP.HttpExceptionContent -> HTTP.HttpException
throwException exceptionContent =
  let FakeHttpRequest request = defaultRequest
  in HTTP.HttpExceptionRequest request exceptionContent

newtype FakeHttpRequest = FakeHttpRequest HTTP.Request

defaultRequest :: FakeHttpRequest
defaultRequest =
  FakeHttpRequest . IO.unsafePerformIO $ HTTP.parseRequest "GET http://foo.com"

defaultRequestComputationInput :: RequestComputationInput
defaultRequestComputationInput =
  RequestComputationInput { _requestComputationInputMethod = Http.Get
                          , _requestComputationInputHeaders = []
                          , _requestComputationInputScheme = Http.Http
                          , _requestComputationInputUrl = "foo.com"
                          , _requestComputationInputBody = ""
                          }

withHttpMock :: [ (FakeHttpRequest, HttpResponse BSU.ByteString) ] -> IO Application
withHttpMock mock = do
  env <- defaultEnv2 <&> envHttpRequest .~ requestRunnerMock (Map.fromList mock)
  mkApp env
  where
    requestRunnerMock
      :: IO.MonadIO m
      => Map FakeHttpRequest (HttpResponse BSU.ByteString)
      -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    requestRunnerMock mock =
      \input -> return $ Map.findWithDefault notFoundResponse (FakeHttpRequest input) mock
        where
          notFoundResponse =
            HttpResponse { httpResponseStatus =
                           HTTP.Status { statusCode = 404
                                       , statusMessage = BSU.fromString "not found"
                                       }
                         , httpResponseHeaders = []
                         , httpResponseBody = BSU.fromString ""
                         }

withExceptionHttpMock :: HTTP.HttpException -> IO Application
withExceptionHttpMock mock = do
  env <- defaultEnv2 <&> envHttpRequest .~ exceptionRunnerMock mock
  mkApp env
  where
    exceptionRunnerMock :: HTTP.HttpException -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    exceptionRunnerMock exception =
      Exception.throw exception

instance Ord FakeHttpRequest where
  FakeHttpRequest r1 `compare` FakeHttpRequest r2 =
    HTTP.host r1 `compare` HTTP.host r2

instance Eq FakeHttpRequest where
   FakeHttpRequest r1 == FakeHttpRequest r2 =
     HTTP.method r1 == HTTP.method r2 &&
     HTTP.secure r1 == HTTP.secure r2 &&
     HTTP.host r1 == HTTP.host r2 &&
     HTTP.port r1 == HTTP.port r2 &&
     HTTP.path r1 == HTTP.path r2 &&
     HTTP.queryString r1 == HTTP.queryString r2 &&
     HTTP.requestHeaders r1 == HTTP.requestHeaders r2
