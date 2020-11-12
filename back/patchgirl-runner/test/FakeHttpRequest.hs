module FakeHttpRequest ( throwException
                       , withHttpMock
                       , withExceptionHttpMock
                       , defaultRequest
                       , defaultRequestComputationInput
                       , buildRequest
                       , buildHttpResponse
                       ) where

import qualified Control.Exception        as Exception
import           Control.Lens.Operators   ((.~))
import qualified Control.Monad            as Monad
import qualified Control.Monad.IO.Class   as IO
import qualified Data.ByteString.UTF8     as BSU
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import           Servant

import           Env
import           Helper.App
import           Interpolator
import           PatchGirl.Web.Http
import           RequestComputation.Model
import           Server


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
defaultRequest = do
  FakeHttpRequest <$> HTTP.parseRequest "GET http://foo.com"

buildHttpResponse :: HttpResponse BSU.ByteString
buildHttpResponse =
  HttpResponse { httpResponseRequestHeaders  = [] -- todo fix this
               , httpResponseRequestBody     = ""
               , httpResponseResponseStatus = HTTP.Status { statusCode = 200
                                                          , statusMessage = BSU.fromString "ok"
                                                          }
               , httpResponseResponseHeaders = []
               , httpResponseResponseBody = BSU.fromString ""
               }

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
                                   , _templatedRequestComputationInputUrl = [ Sentence "http://foo.com" ]
                                   , _templatedRequestComputationInputBody = [ Sentence "" ]
                                   }


-- * exception mock


withExceptionHttpMock :: IO HTTP.HttpException -> IO Application
withExceptionHttpMock mock = do
  mock' <- mock
  env <- envWithLog <&> envHttpRequest .~ exceptionRunnerMock mock'
  mkApp env
  where
    exceptionRunnerMock :: HTTP.HttpException -> (HTTP.CookieJar -> HTTP.Request -> m (HTTP.CookieJar, HttpResponse BSU.ByteString))
    exceptionRunnerMock exception =
      Exception.throw exception


-- * with http mock


withHttpMock
  :: [ ( IO FakeHttpRequest
       , Either HTTP.HttpException (HttpResponse BSU.ByteString)
       )
     ]
  -> IO Application
withHttpMock rawMock = do
  mock <- Monad.forM rawMock unwrapIO
  env <- envWithLog <&> envHttpRequest .~ requestRunnerMock (Map.fromList mock)
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
      -> (HTTP.CookieJar -> HTTP.Request -> m (HTTP.CookieJar, HttpResponse BSU.ByteString))
    requestRunnerMock mock =
      \cookieJar request -> do
        Map.findWithDefault (Right notFoundResponse) (FakeHttpRequest request) mock & \case
          Left exception -> Exception.throw exception
          Right response -> return (cookieJar, response)
        where
          notFoundResponse =
            HttpResponse { httpResponseRequestHeaders  = []
                         , httpResponseRequestBody     = ""
                         , httpResponseResponseStatus = HTTP.Status { statusCode = 404
                                                                    , statusMessage = BSU.fromString "not found"
                                                                    }
                         , httpResponseResponseHeaders = []
                         , httpResponseResponseBody = BSU.fromString ""
                         }
