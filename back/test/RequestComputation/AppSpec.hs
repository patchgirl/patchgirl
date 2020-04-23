{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module RequestComputation.AppSpec where

import           App
import qualified Data.ByteString.UTF8     as BSU
import           FakeHttpRequest
import           Helper.App
import qualified Http
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP

import           RequestComputation.Model
import           Servant
import qualified Servant.Auth.Client      as Auth
import           Servant.Auth.Server      (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


runRequestComputation :: Auth.Token -> RequestComputationInput -> ClientM RequestComputationResult
runRequestComputation =
  client (Proxy :: Proxy (RequestComputationApi '[JWT]))


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  describe "valid request computation input" $
    withClient (withHttpMock [ requestWithResponse1 ]) $
      it "returns ok200" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token input1) `shouldReturn` output1


-- ** computation failed


  describe "invalid url" $
    withClient (withExceptionHttpMock (pure $ HTTP.InvalidUrlException "" "")) $
      it "returns invalid url exception" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token defaultRequestComputationInput) `shouldReturn` (RequestComputationFailed $ InvalidUrlException "" "")

  describe "too many redirects" $
    withClient (withExceptionHttpMock (throwException $ HTTP.TooManyRedirects [])) $
      it "returns too many redirects" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token defaultRequestComputationInput) `shouldReturn` RequestComputationFailed TooManyRedirects

  describe "connection timeout" $
    withClient (withExceptionHttpMock (throwException HTTP.ConnectionTimeout)) $
      it "returns overlong headers" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token defaultRequestComputationInput) `shouldReturn` RequestComputationFailed ConnectionTimeout


  where


-- ** 200


    requestWithResponse1 =
      ( defaultRequest
      , HttpResponse { httpResponseStatus =
                       HTTP.Status { statusCode = 200
                                   , statusMessage = BSU.fromString "ok"
                                   }
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
      , RequestComputationSucceeded (RequestComputationOutput { _requestComputationOutputStatusCode = 200
                                                              , _requestComputationOutputHeaders    = []
                                                              , _requestComputationOutputBody       = ""
                                                              })
      )
