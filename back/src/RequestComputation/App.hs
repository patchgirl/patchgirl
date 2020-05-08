{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module RequestComputation.App ( runRequestComputationHandler
                              , ioRequestRunner
                              ) where


import qualified Control.Exception           as Exception
import qualified Control.Monad.IO.Class      as IO
import qualified Control.Monad.Reader        as Reader
import qualified Data.ByteString.UTF8        as BSU
import qualified Data.CaseInsensitive        as CI
import           Data.Functor                ((<&>))
import qualified Data.List                   as List
import qualified Network.HTTP.Client.Conduit as Http
import qualified Network.HTTP.Simple         as Http
import qualified Network.HTTP.Types          as Http

import           Environment.Model
import           Http
import           Interpolator
import           PatchGirl
import           RequestComputation.Model


-- * handler


runRequestComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => (TemplatedRequestComputationInput, RequestEnvironment)
  -> m RequestComputationResult
runRequestComputationHandler (templatedRequestComputationInput, requestEnvironment) = do
  runner <- Reader.ask <&> _envHttpRequest
  IO.liftIO $
    Exception.try (
      buildRequest templatedRequestComputationInput requestEnvironment >>= runner
    ) <&> responseToComputationResult


-- * build request


buildRequest :: TemplatedRequestComputationInput -> RequestEnvironment -> IO Http.Request
buildRequest templatedRequestComputationInput requestEnvironment = do
  let RequestComputationInput{..} = buildRequestComputationInput templatedRequestComputationInput requestEnvironment
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


-- * interpolate request environment


buildRequestComputationInput :: TemplatedRequestComputationInput -> RequestEnvironment -> RequestComputationInput
buildRequestComputationInput TemplatedRequestComputationInput{..} requestEnvironment =
  RequestComputationInput { _requestComputationInputMethod = _templatedRequestComputationInputMethod
                          , _requestComputationInputHeaders =
                            _templatedRequestComputationInputHeaders <&> \(h, v) -> ( toString $ interpolate h
                                                                                    , toString $ interpolate v
                                                                                    )
                          , _requestComputationInputScheme = _templatedRequestComputationInputScheme
                          , _requestComputationInputUrl = toString $ interpolate _templatedRequestComputationInputUrl
                          , _requestComputationInputBody = toString $ interpolate _templatedRequestComputationInputBody
                          }
  where
    interpolate :: [TemplatedString] -> [TemplatedString]
    interpolate =
      map (interpolateRequestEnvironment requestEnvironment)

    toString :: [TemplatedString] -> String
    toString templatedStrings =
      List.intercalate "" $ map templatedStringToString templatedStrings


-- * run request


ioRequestRunner :: Http.Request -> IO (HttpResponse BSU.ByteString)
ioRequestRunner request =
  Http.httpBS request <&> fromResponseToHttpResponse


-- * create request computation output


responseToComputationResult :: Either Http.HttpException (HttpResponse BSU.ByteString) -> Either HttpException RequestComputationOutput
responseToComputationResult = \case
    Right response ->
      Right $
        RequestComputationOutput { _requestComputationOutputStatusCode = Http.statusCode $ httpResponseStatus response
                                 , _requestComputationOutputHeaders    = parseResponseHeaders response
                                 , _requestComputationOutputBody       = BSU.toString $ httpResponseBody response
                                 }

    Left (Http.InvalidUrlException url reason) ->
      Left (InvalidUrlException url reason)

    Left (Http.HttpExceptionRequest _ content) ->
      Left matching
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
          Http.InternalException _ -> InternalException
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
