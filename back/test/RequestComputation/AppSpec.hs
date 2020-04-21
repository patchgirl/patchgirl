{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module RequestComputation.AppSpec where

import           App
import           Control.Exception           (Exception, SomeException)
import qualified Control.Exception           as Exception
import           Control.Lens.Operators      ((&), (.~), (^.))
import qualified Control.Monad.IO.Class      as IO
import qualified Data.ByteString.UTF8        as BSU
import           Data.Functor                ((<&>))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Debug.Trace
import           Env
import           Helper.App
import qualified Http
import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple         as HTTP
import qualified Network.HTTP.Types          as HTTP
import qualified Network.HTTP.Types.Header   as HTTP
import qualified Network.HTTP.Types.Version  as HTTP
import           System.IO.Unsafe            as IO

import           RequestCollection.DB
import           RequestCollection.Model
import           RequestComputation.Model
import           Servant
import qualified Servant.Auth.Client         as Auth
import           Servant.Auth.Server         (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


runRequestComputation :: Auth.Token -> RequestComputationInput -> ClientM RequestComputationResult
runRequestComputation =
  client (Proxy :: Proxy (RequestComputationApi '[JWT]))


-- * spec


spec :: Spec
spec = do
  describe "valid request computation input" $ do
    withClient (withHttpMock [ requestWithResponse1 ]) $ do
      it "returns ok200" $ \clientEnv -> do
        createAccountAndcleanDBAfter $ \Test { token } -> do
          try clientEnv (runRequestComputation token input1) `shouldReturn` output1


  describe "invalid url" $ do
    withClient (withExceptionHttpMock (HTTP.InvalidUrlException "" "")) $ do
      it "returns ok200" $ \clientEnv -> do
        createAccountAndcleanDBAfter $ \Test { token } -> do
          try clientEnv (runRequestComputation token inputInvalidUrl) `shouldReturn` outputInvalidUrl


  where


-- ** 200


    requestWithResponse1 =
      ( IO.unsafePerformIO $ buildRequest "GET" "http://foo.com" ""
      , HttpResponse { httpResponseStatus = HTTP.Status { statusCode = 200, statusMessage = BSU.fromString "ok" }
                     , httpResponseHeaders = []
                     , httpResponseBody = BSU.fromString ""
                     }
      )

    (input1, output1) =
      ( RequestComputationInput { _requestComputationInputMethod = Http.Get
                                , _requestComputationInputHeaders = []
                                , _requestComputationInputScheme = Http.Http
                                , _requestComputationInputUrl = "foo.com"
                                , _requestComputationInputBody = ""
                                }
      , GotRequestComputationOutput RequestComputationOutput { _requestComputationOutputStatusCode = 200
                                                             , _requestComputationOutputHeaders    = []
                                                             , _requestComputationOutputBody       = ""
                                                             }
      )


-- ** invalid url


    (inputInvalidUrl, outputInvalidUrl) =
      ( RequestComputationInput { _requestComputationInputMethod = Http.Get
                                , _requestComputationInputHeaders = []
                                , _requestComputationInputScheme = Http.Http
                                , _requestComputationInputUrl = "some weird url"
                                , _requestComputationInputBody = ""
                                }
      , RequestBadUrl
      )


-- ** util


    withHttpMock mock = do
      env <- defaultEnv2 <&> envHttpRequest .~ (requestRunnerMock (Map.fromList mock))
      mkApp env

    withExceptionHttpMock mock = do
      env <- defaultEnv2 <&> envHttpRequest .~ (exceptionRunnerMock mock)
      mkApp env

    requestRunnerMock
      :: IO.MonadIO m
      => Map HTTP.Request (HttpResponse BSU.ByteString)
      -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    requestRunnerMock mock = do
        \input -> return $ Map.findWithDefault notFoundResponse input mock
      where
        notFoundResponse =
          HttpResponse { httpResponseStatus = HTTP.Status { statusCode = 404
                                                          , statusMessage = BSU.fromString "not found"
                                                          }
                       , httpResponseHeaders = []
                       , httpResponseBody = BSU.fromString ""
                       }

    exceptionRunnerMock :: (IO.MonadIO m) => HTTP.HttpException -> (HTTP.Request -> m (HttpResponse BSU.ByteString))
    exceptionRunnerMock exception = do
      Exception.throw exception

    buildRequest :: String -> String -> String -> IO HTTP.Request
    buildRequest method url body = do
        parsedRequest <- IO.liftIO $ HTTP.parseRequest url
        return
          -- $ Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
          -- $ setPortAndSecure
          $ HTTP.setRequestBody (HTTP.RequestBodyBS $ BSU.fromString body)
          $ HTTP.setRequestMethod (BSU.fromString method)
          parsedRequest

instance Eq HTTP.Request where
   r1 == r2 =
     HTTP.method r1 == HTTP.method r2 &&
     HTTP.secure r1 == HTTP.secure r2 &&
     HTTP.host r1 == HTTP.host r2 &&
     HTTP.port r1 == HTTP.port r2 &&
     HTTP.path r1 == HTTP.path r2 &&
     HTTP.queryString r1 == HTTP.queryString r2 &&
     HTTP.requestHeaders r1 == HTTP.requestHeaders r2

instance Ord HTTP.Request
