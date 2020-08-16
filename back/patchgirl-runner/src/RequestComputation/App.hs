{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module RequestComputation.App ( runRequestComputationHandler
                              , runRequestComputationWithScenarioContext
                              , ioRequestRunner
                              ) where


import qualified Control.Exception           as Exception
import qualified Control.Monad.IO.Class      as IO
import qualified Control.Monad.Reader        as Reader
import qualified Control.Monad.State         as State
import qualified Data.Bifunctor              as Bifunctor
import qualified Data.ByteString.UTF8        as BSU
import qualified Data.CaseInsensitive        as CI
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import qualified Data.Map.Strict             as Map
import qualified Network.HTTP.Client.Conduit as Http
import qualified Network.HTTP.Simple         as Http
import qualified Network.HTTP.Types          as Http

import           Env
import           Http
import           Interpolator
import           Log
import           RequestComputation.Model
import           ScenarioComputation.Model


-- * handler


runRequestComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => (TemplatedRequestComputationInput, EnvironmentVars)
  -> m RequestComputationOutput
runRequestComputationHandler (templatedRequestComputationInput, environmentVars) =
--  runRequestComputationWithScenarioContext templatedRequestComputationInput environmentVars Map.empty Map.empty
  State.evalStateT (runRequestComputationWithScenarioContext templatedRequestComputationInput) (ScriptContext environmentVars Map.empty Map.empty)


-- * run request computation with scenario context


runRequestComputationWithScenarioContext
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , State.MonadState ScriptContext m
     )
  => TemplatedRequestComputationInput
  -> m RequestComputationOutput
runRequestComputationWithScenarioContext templatedRequestComputationInput = do
  runner <- Reader.ask <&> _envHttpRequest
  ScriptContext{..} <- State.get
  result <- IO.liftIO $
    Exception.try (
      buildRequest templatedRequestComputationInput environmentVars globalVars localVars >>= runner
    )
  responseToComputationResult result


-- * build request


buildRequest
  :: TemplatedRequestComputationInput
  -> EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> IO Http.Request
buildRequest templatedRequestComputationInput environmentVars scenarioGlobalVars scenarioLocalVars = do
  buildRequestComputationInput templatedRequestComputationInput environmentVars scenarioGlobalVars scenarioLocalVars
    & \RequestComputationInput{..} -> Http.parseRequest _requestComputationInputUrl
    <&> Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
    <&> Http.setRequestBody (Http.RequestBodyBS $ BSU.fromString _requestComputationInputBody)
    <&> Http.setRequestMethod (BSU.fromString $ methodToString _requestComputationInputMethod)
    <&> setPortAndSecure _requestComputationInputUrl
  where
    setPortAndSecure :: String -> Http.Request -> Http.Request
    setPortAndSecure = \case
        ('h' : 't' : 't' : 'p' : 's' : _) ->
          Http.setRequestSecure True . Http.setRequestPort 443

        _ ->
          Http.setRequestSecure False . Http.setRequestPort 80


-- * build request computation input


buildRequestComputationInput
  :: TemplatedRequestComputationInput
  -> EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> RequestComputationInput
buildRequestComputationInput TemplatedRequestComputationInput{..} environmentVars scenarioGlobalVars scenarioLocalVars =
  RequestComputationInput { _requestComputationInputMethod = _templatedRequestComputationInputMethod
                          , _requestComputationInputHeaders =
                            _templatedRequestComputationInputHeaders <&> Bifunctor.bimap interpolate' interpolate'
                          , _requestComputationInputUrl = interpolate' _templatedRequestComputationInputUrl
                          , _requestComputationInputBody = interpolate' _templatedRequestComputationInputBody
                          }
  where
    interpolate' = interpolate environmentVars scenarioGlobalVars scenarioLocalVars


-- * run request


ioRequestRunner :: Http.Request -> IO (HttpResponse BSU.ByteString)
ioRequestRunner request =
  Http.httpBS request <&> fromResponseToHttpResponse


-- * create request computation output


responseToComputationResult
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Either Http.HttpException (HttpResponse BSU.ByteString)
  -> m (Either HttpException RequestComputation)
responseToComputationResult either = do
  case either of
    Right response ->
      return . Right $
        RequestComputation { _requestComputationStatusCode = Http.statusCode $ httpResponseStatus response
                                 , _requestComputationHeaders    = parseResponseHeaders response
                                 , _requestComputationBody       = BSU.toString $ httpResponseBody response
                                 }

    Left ex@(Http.InvalidUrlException url reason) -> do
      logError $ show ex
      return $ Left (InvalidUrlException url reason)

    Left ex@(Http.HttpExceptionRequest _ content) -> do
      logError $ show ex
      return $ Left matching
      where
        matching = case content of
          Http.TooManyRedirects _ -> TooManyRedirects
          Http.OverlongHeaders -> OverlongHeaders
          Http.ResponseTimeout -> ResponseTimeout
          Http.ConnectionTimeout -> ConnectionTimeout
          Http.ConnectionFailure f -> ConnectionFailure (show f)
          Http.InvalidStatusLine _ -> InvalidStatusLine
          Http.InvalidHeader _-> InvalidHeader
          Http.InvalidRequestHeader _ -> InvalidRequestHeader
          Http.InternalException someException -> InternalException (show someException)
          Http.ProxyConnectException {} -> ProxyConnectException
          Http.NoResponseDataReceived -> NoResponseDataReceived
          Http.WrongRequestBodyStreamSize _ _ -> WrongRequestBodyStreamSize
          Http.ResponseBodyTooShort _ _ -> ResponseBodyTooShort
          Http.InvalidChunkHeaders -> InvalidChunkHeaders
          Http.IncompleteHeaders -> IncompleteHeaders
          Http.InvalidDestinationHost _ -> InvalidDestinationHost
          Http.HttpZlibException _ -> HttpZlibException
          Http.InvalidProxyEnvironmentVariable _ _ -> InvalidProxyEnvironmentVariable
          Http.ConnectionClosed -> ConnectionClosed
          Http.InvalidProxySettings _ -> InvalidProxySettings
          _ -> UnknownException


parseResponseHeaders :: HttpResponse BSU.ByteString -> [(String,String)]
parseResponseHeaders response =
  map convert (httpResponseHeaders response)
  where
    convert :: (Http.HeaderName, BSU.ByteString) -> (String, String)
    convert (headerKey, headerValue) =
      ( BSU.toString $ CI.original headerKey
      , BSU.toString headerValue
      )

mkHeader :: (String, String) -> Http.Header
mkHeader (headerName, headerValue) =
  (CI.mk $ BSU.fromString headerName, BSU.fromString headerValue)
