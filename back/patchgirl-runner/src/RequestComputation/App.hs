{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module RequestComputation.App ( runRequestComputationHandler
                              , runRequestComputationWithScenarioContext
                              , ioRequestRunner
                              ) where


import qualified Control.Exception           as Exception
import qualified Control.Monad               as Monad
import qualified Control.Monad.IO.Class      as IO
import qualified Control.Monad.Reader        as Reader
import qualified Control.Monad.State         as State
import qualified Data.ByteString.UTF8        as BSU
import qualified Data.CaseInsensitive        as CI
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import qualified Data.Time                   as Time
import qualified Network.HTTP.Client.Conduit as Http
import qualified Network.HTTP.Simple         as Http
import qualified Network.HTTP.Types          as Http

import           Env
import           Interpolator
import           PatchGirl.Web.Http
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
  let scenarioContext = emptyScenarioContext { _scenarioContextEnvironmentVars = environmentVars }
  in State.evalStateT (runRequestComputationWithScenarioContext templatedRequestComputationInput) scenarioContext


-- * run request computation with scenario context


runRequestComputationWithScenarioContext
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , State.MonadState ScenarioContext m
     )
  => TemplatedRequestComputationInput
  -> m RequestComputationOutput
runRequestComputationWithScenarioContext templatedRequestComputationInput = do
  runner <- Reader.ask <&> _envHttpRequest
  request <- buildRequest templatedRequestComputationInput
  cookieJar <- State.get <&> _scenarioContextCookieJar
  result <- IO.liftIO $ Exception.try $ runner cookieJar request

  case result of
    Right (newCookieJar, _) ->
      State.modify $ \s -> s { _scenarioContextCookieJar = newCookieJar }

    _ -> return ()

  responseToComputationResult (result <&> snd)


-- * build request


buildRequest
  :: ( IO.MonadIO m
     , State.MonadState ScenarioContext m
     )
  => TemplatedRequestComputationInput
  -> m Http.Request
buildRequest templatedRequestComputationInput = do
  RequestComputationInput{..} <- buildRequestComputationInput templatedRequestComputationInput
  requestWithoutCookie <- IO.liftIO $
    Http.parseRequest _requestComputationInputUrl
    <&> Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
    <&> Http.setRequestBody (Http.RequestBodyBS $ BSU.fromString _requestComputationInputBody)
    <&> Http.setRequestMethod (BSU.fromString $ methodToString _requestComputationInputMethod)
  setCookie requestWithoutCookie

  where
    {-| fetch the latest cookie and add it to the current request, also update the `last-access-time` value of the cookieJar -}
    setCookie
      :: ( IO.MonadIO m
         , State.MonadState ScenarioContext m
         )
      => Http.Request
      -> m Http.Request
    setCookie request = do
      cookieJar <- State.get <&> _scenarioContextCookieJar
      time <- IO.liftIO Time.getCurrentTime
      let (newRequest, newCookieJar) = Http.insertCookiesIntoRequest request cookieJar time
      State.modify $ \s -> s { _scenarioContextCookieJar = newCookieJar }
      return newRequest


-- * build request computation input


buildRequestComputationInput
  :: State.MonadState ScenarioContext m
  => TemplatedRequestComputationInput
  -> m RequestComputationInput
buildRequestComputationInput TemplatedRequestComputationInput{..} = do
  url <- interpolate' _templatedRequestComputationInputUrl
  body <- interpolate' _templatedRequestComputationInputBody
  headers :: [(String, String)] <- Monad.forM _templatedRequestComputationInputHeaders $ \(oldK, oldV) -> do
    k <- interpolate' oldK
    v <- interpolate' oldV
    return (k, v)
  return $ RequestComputationInput { _requestComputationInputMethod = _templatedRequestComputationInputMethod
                                   , _requestComputationInputHeaders = headers
                                   , _requestComputationInputUrl = url
                                   , _requestComputationInputBody = body
                                   }
  where
    interpolate'
      :: State.MonadState ScenarioContext m
      => StringTemplate
      -> m String
    interpolate' st = do
      sceneVars <- State.get <&> _scenarioContextSceneVars
      envVars <- State.get <&> _scenarioContextEnvironmentVars
      globalVars <- State.get <&> _scenarioContextGlobalVars
      localVars <- State.get <&> _scenarioContextLocalVars
      return $ interpolate sceneVars envVars globalVars localVars st


-- * run request


ioRequestRunner :: Http.CookieJar -> Http.Request -> IO (Http.CookieJar, HttpResponse BSU.ByteString)
ioRequestRunner cookieJar request = do
  response <- Http.httpBS request
  now <- Time.getCurrentTime
  let newCookieJar = Http.updateCookieJar response request now cookieJar & fst
  return ( newCookieJar
         , fromResponseToHttpResponse request response
         )


-- * response to computation result


responseToComputationResult
  :: ( Reader.MonadReader Env m )
  => Either Http.HttpException (HttpResponse BSU.ByteString)
  -> m (Either HttpException RequestComputation)
responseToComputationResult either = do
  case either of
    Right response ->
      return . Right $
        RequestComputation { _requestComputationRequestHeaders = httpResponseRequestHeaders response
                           , _requestComputationRequestBody = httpResponseRequestBody response
                           , _requestComputationResponseStatusCode = Http.statusCode $ httpResponseResponseStatus response
                           , _requestComputationResponseHeaders    = parseResponseHeaders response
                           , _requestComputationResponseBody       = BSU.toString $ httpResponseResponseBody response
                           }

    Left (Http.InvalidUrlException url reason) -> do
      return $ Left (InvalidUrlException url reason)

    Left (Http.HttpExceptionRequest _ content) -> do
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
  map convert (httpResponseResponseHeaders response)
  where
    convert :: (Http.HeaderName, BSU.ByteString) -> (String, String)
    convert (headerKey, headerValue) =
      ( BSU.toString $ CI.original headerKey
      , BSU.toString headerValue
      )

mkHeader :: (String, String) -> Http.Header
mkHeader (headerName, headerValue) =
  (CI.mk $ BSU.fromString headerName, BSU.fromString headerValue)
