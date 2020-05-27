module FakeHttpRequest ( throwException
                       , withHttpMock
                       , withHttpMock2
                       , withExceptionHttpMock
                       , defaultRequest
                       , defaultRequestComputationInput
                       , buildRequest
                       ) where

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

import           Helper.App
import           PatchGirl.Internal
import           PatchGirl.Server


-- * fake request


newtype FakeHttpRequest = FakeHttpRequest HTTP.Request deriving (Show)

instance Ord FakeHttpRequest where
  FakeHttpRequest r1 `compare` FakeHttpRequest r2 =
    show r1 `compare` show r2

instance Eq FakeHttpRequest where
   FakeHttpRequest r1 == FakeHttpRequest r2 =
     HTTP.method r1 == HTTP.method r2 &&
     HTTP.secure r1 == HTTP.secure r2 &&
     HTTP.host r1 == HTTP.host r2 &&
     HTTP.port r1 == HTTP.port r2 &&
     HTTP.path r1 == HTTP.path r2 &&
     HTTP.queryString r1 == HTTP.queryString r2 &&
     HTTP.requestHeaders r1 == HTTP.requestHeaders r2

buildRequest :: String -> IO FakeHttpRequest
buildRequest rawRequest =
  FakeHttpRequest <$> HTTP.parseRequest rawRequest

defaultRequest :: IO FakeHttpRequest
defaultRequest =
  FakeHttpRequest <$> HTTP.parseRequest "GET http://foo.com"


-- * exception util


throwException :: HTTP.HttpExceptionContent -> IO HTTP.HttpException
throwException exceptionContent = do
  FakeHttpRequest request <- defaultRequest
  return $ HTTP.HttpExceptionRequest request exceptionContent


-- * request computation input util


defaultRequestComputationInput :: TemplatedRequestComputationInput
defaultRequestComputationInput =
  TemplatedRequestComputationInput { _templatedRequestComputationInputMethod = Get
                                   , _templatedRequestComputationInputHeaders = []
                                   , _templatedRequestComputationInputScheme = Http
                                   , _templatedRequestComputationInputUrl = [ Sentence "foo.com" ]
                                   , _templatedRequestComputationInputBody = [ Sentence "" ]
                                   }


-- * mock


withHttpMock :: [ (IO FakeHttpRequest, HttpResponse BSU.ByteString) ] -> IO Application
withHttpMock rawMock = do
  mock <- Monad.forM rawMock foo
  env <- defaultEnv2 <&> envHttpRequest .~ requestRunnerMock (Map.fromList mock)
  mkApp env
  where
    foo :: (IO FakeHttpRequest, HttpResponse BSU.ByteString) -> IO (FakeHttpRequest, HttpResponse BSU.ByteString)
    foo (f, s) = f <&> \f'-> (f', s)

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


-- * exception mock


withExceptionHttpMock :: IO HTTP.HttpException -> IO Application
withExceptionHttpMock mock = do
  mock' <- mock
  env <- defaultEnv2 <&> envHttpRequest .~ exceptionRunnerMock mock'
  mkApp env
  where
    exceptionRunnerMock :: HTTP.HttpException -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    exceptionRunnerMock exception =
      Exception.throw exception


-- * with http mock


withHttpMock2
  :: [ ( IO FakeHttpRequest
       , Either HTTP.HttpException (HttpResponse BSU.ByteString)
       )
     ]
  -> IO Application
withHttpMock2 rawMock = do
  mock <- Monad.forM rawMock unwrapIO
  env <- defaultEnv2 <&> envHttpRequest .~ requestRunnerMock (Map.fromList mock)
  mkApp env
  where
    unwrapIO
      :: ( IO FakeHttpRequest
         , Either HTTP.HttpException (HttpResponse BSU.ByteString)
         )
      -> IO ( FakeHttpRequest
            , Either HTTP.HttpException (HttpResponse BSU.ByteString)
            )
    unwrapIO (f, s) = f <&> \f'-> (f', s)

    requestRunnerMock
      :: IO.MonadIO m
      => Map FakeHttpRequest (Either HTTP.HttpException (HttpResponse BSU.ByteString))
      -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    requestRunnerMock mock =
      \input -> do
        let either = Map.findWithDefault (Right notFoundResponse) (FakeHttpRequest input) mock
        case either of
          Left exception -> Exception.throw exception
          Right response -> return response
        where
          notFoundResponse =
            HttpResponse { httpResponseStatus =
                           HTTP.Status { statusCode = 404
                                       , statusMessage = BSU.fromString "not found"
                                       }
                         , httpResponseHeaders = []
                         , httpResponseBody = BSU.fromString ""
                         }
