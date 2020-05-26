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
import qualified Data.ByteString.UTF8        as BSU
import qualified Data.CaseInsensitive        as CI
import           Data.Functor                ((<&>))
import qualified Data.Map.Strict             as Map
import qualified Network.HTTP.Client.Conduit as Http
import qualified Network.HTTP.Simple         as Http
import qualified Network.HTTP.Types          as Http

import           Http
import           Interpolator
import           PatchGirl
import           RequestComputation.Model


-- * handler


runRequestComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => (TemplatedRequestComputationInput, EnvironmentVars)
  -> m RequestComputationResult
runRequestComputationHandler (templatedRequestComputationInput, environmentVars) = do
  runner <- Reader.ask <&> _envHttpRequest
  result <- IO.liftIO $
    Exception.try (
      buildRequest templatedRequestComputationInput environmentVars Map.empty Map.empty >>= runner
    )
  responseToComputationResult result


-- * run request computation with scenario context


runRequestComputationWithScenarioContext
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => TemplatedRequestComputationInput
  -> EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> m RequestComputationResult
runRequestComputationWithScenarioContext templatedRequestComputationInput environmentVars scenarioGlobalVars scenarioLocalVars = do
  runner <- Reader.ask <&> _envHttpRequest
  result <- IO.liftIO $
    Exception.try (
      buildRequest templatedRequestComputationInput environmentVars scenarioGlobalVars scenarioLocalVars >>= runner
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
  let RequestComputationInput{..} = buildRequestComputationInput templatedRequestComputationInput environmentVars scenarioGlobalVars scenarioLocalVars
  let url = schemeToString _requestComputationInputScheme  <> "://" <> _requestComputationInputUrl
  parsedRequest <- IO.liftIO $ Http.parseRequest url
  return
    $ Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
    $ setPortAndSecure _requestComputationInputScheme
    $ Http.setRequestBody (Http.RequestBodyBS $ BSU.fromString _requestComputationInputBody)
    $ Http.setRequestMethod (BSU.fromString $ methodToString _requestComputationInputMethod)
    parsedRequest
  where
    setPortAndSecure :: Scheme -> Http.Request -> Http.Request
    setPortAndSecure = \case
        Http ->
          Http.setRequestSecure False . Http.setRequestPort 80

        Https ->
          Http.setRequestSecure True . Http.setRequestPort 443


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
                            _templatedRequestComputationInputHeaders <&> \(h, v) -> (interpolate' h, interpolate' v)
                          , _requestComputationInputScheme = _templatedRequestComputationInputScheme
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
  -> m (Either HttpException RequestComputationOutput)
responseToComputationResult either = do
  case either of
    Right response ->
      return . Right $
        RequestComputationOutput { _requestComputationOutputStatusCode = Http.statusCode $ httpResponseStatus response
                                 , _requestComputationOutputHeaders    = parseResponseHeaders response
                                 , _requestComputationOutputBody       = BSU.toString $ httpResponseBody response
                                 }

    Left (ex@(Http.InvalidUrlException url reason)) -> do
      logError $ show ex
      return $ Left (InvalidUrlException url reason)

    Left (ex@(Http.HttpExceptionRequest _ content)) -> do
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
