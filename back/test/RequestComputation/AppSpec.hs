{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module RequestComputation.AppSpec where

import qualified Data.ByteString.UTF8     as BSU
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import           Servant
import qualified Servant.Auth.Client      as Auth
import           Servant.Auth.Server      (JWT)
import           Servant.Client
import           Test.Hspec

import           App
import           Environment.Model
import           FakeHttpRequest
import           Helper.App
import qualified Http
import           RequestComputation.Model
import           TangoScript


-- * client


runRequestComputation
  :: Auth.Token
  -> (TemplatedRequestComputationInput, RequestEnvironment)
  -> ClientM RequestComputationResult
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
          try clientEnv (runRequestComputation token (input1, requestEnvironment)) `shouldReturn` output1


-- ** computation failed


  describe "invalid url" $
    withClient (withExceptionHttpMock (pure $ HTTP.InvalidUrlException "" "")) $
      it "returns invalid url exception" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token (defaultRequestComputationInput, requestEnvironment)) `shouldReturn` Left (InvalidUrlException "" "")

  describe "too many redirects" $
    withClient (withExceptionHttpMock (throwException $ HTTP.TooManyRedirects [])) $
      it "returns too many redirects" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token (defaultRequestComputationInput, requestEnvironment)) `shouldReturn` Left TooManyRedirects

  describe "connection timeout" $
    withClient (withExceptionHttpMock (throwException HTTP.ConnectionTimeout)) $
      it "returns overlong headers" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runRequestComputation token (defaultRequestComputationInput, requestEnvironment)) `shouldReturn` Left ConnectionTimeout


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
      ( TemplatedRequestComputationInput { _templatedRequestComputationInputMethod = Http.Get
                                         , _templatedRequestComputationInputHeaders = []
                                         , _templatedRequestComputationInputScheme = Http.Http
                                         , _templatedRequestComputationInputUrl = [ Sentence "foo.com" ]
                                         , _templatedRequestComputationInputBody = [ Sentence "" ]
                                         }
      , Right (RequestComputationOutput { _requestComputationOutputStatusCode = 200
                                        , _requestComputationOutputHeaders    = []
                                        , _requestComputationOutputBody       = ""
                                        })
      )


-- ** scenario environment


requestEnvironment :: Map String String
requestEnvironment = Map.fromList []
