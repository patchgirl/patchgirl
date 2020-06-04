{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module RequestComputation.AppSpec where

import qualified Data.ByteString.UTF8     as BSU
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import           Servant
import qualified Servant.Client           as Servant
import           Test.Hspec

import           Api
import           FakeHttpRequest
import           Helper.App
import           Http
import           Interpolator
import           RequestComputation.Model


-- * client


runRequestComputation :: (TemplatedRequestComputationInput, EnvironmentVars) -> Servant.ClientM RequestComputationResult
runRequestComputation =
  Servant.client (Proxy :: Proxy RequestComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  describe "valid request computation input" $
    withClient (withHttpMock [ requestWithResponse1 ]) $
      it "returns ok200" $ \clientEnv ->
        try clientEnv (runRequestComputation (input1, envVars)) `shouldReturn` output1


-- ** computation interpolate env vars


  describe "computation interpolate env vars" $
    withClient (withHttpMock [ requestWithResponse2 ]) $
      it "interpolate env vars" $ \clientEnv -> do
        let envVars = Map.fromList [ ( "host"
                                     , [ Sentence "http://foo.com" ]
                                     )
                                   ]
        try clientEnv (runRequestComputation (input2, envVars))
        `shouldReturn` output2


-- ** computation failed


  describe "invalid url" $
    withClient (withExceptionHttpMock (pure $ HTTP.InvalidUrlException "" "")) $
      it "returns invalid url exception" $ \clientEnv ->
        try clientEnv (runRequestComputation (defaultRequestComputationInput, envVars)) `shouldReturn` Left (InvalidUrlException "" "")

  describe "too many redirects" $
    withClient (withExceptionHttpMock (throwException $ HTTP.TooManyRedirects [])) $
      it "returns too many redirects" $ \clientEnv ->
        try clientEnv (runRequestComputation (defaultRequestComputationInput, envVars)) `shouldReturn` Left TooManyRedirects

  describe "connection timeout" $
    withClient (withExceptionHttpMock (throwException HTTP.ConnectionTimeout)) $
      it "returns overlong headers" $ \clientEnv ->
        try clientEnv (runRequestComputation (defaultRequestComputationInput, envVars)) `shouldReturn` Left ConnectionTimeout


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
      ( TemplatedRequestComputationInput { _templatedRequestComputationInputMethod = Get
                                         , _templatedRequestComputationInputHeaders = []
                                         , _templatedRequestComputationInputUrl = [ Sentence "http://foo.com" ]
                                         , _templatedRequestComputationInputBody = [ Sentence "" ]
                                         }
      , Right (RequestComputationOutput { _requestComputationOutputStatusCode = 200
                                        , _requestComputationOutputHeaders    = []
                                        , _requestComputationOutputBody       = ""
                                        })
      )


-- ** 200 with interpolation


    requestWithResponse2 =
      ( defaultRequest
      , HttpResponse { httpResponseStatus =
                       HTTP.Status { statusCode = 200
                                   , statusMessage = BSU.fromString "ok"
                                   }
                     , httpResponseHeaders = []
                     , httpResponseBody = BSU.fromString ""
                     }
      )

    (input2, output2) =
      ( TemplatedRequestComputationInput { _templatedRequestComputationInputMethod = Get
                                         , _templatedRequestComputationInputHeaders = []
                                         , _templatedRequestComputationInputUrl = [ Key "host" ]
                                         , _templatedRequestComputationInputBody = [ Sentence "" ]
                                         }
      , Right (RequestComputationOutput { _requestComputationOutputStatusCode = 200
                                        , _requestComputationOutputHeaders    = []
                                        , _requestComputationOutputBody       = ""
                                        })
      )


-- ** environment vars


envVars :: Map String StringTemplate
envVars = Map.empty
