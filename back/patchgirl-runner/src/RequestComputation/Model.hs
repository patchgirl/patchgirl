{-# LANGUAGE DeriveGeneric #-}

module RequestComputation.Model ( RequestComputationInput(..)
                                , TemplatedRequestComputationInput(..)
                                , RequestComputation(..)
                                , RequestComputationOutput
                                , HttpResponse(..)
                                , fromResponseToHttpResponse
                                , HttpException(..)
                                ) where


import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.CaseInsensitive      as CI
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Client       as Http
import qualified Network.HTTP.Simple       as Http
import qualified Network.HTTP.Types        as Http

import           Interpolator
import           PatchGirl.Web.Http


-- * http response


data HttpResponse body = HttpResponse
    { httpResponseRequestHeaders  :: [(String, String)]
    , httpResponseRequestBody     :: String
    , httpResponseResponseStatus  :: Http.Status
    , httpResponseResponseHeaders :: Http.ResponseHeaders
    , httpResponseResponseBody    :: body
    }
    deriving (Show)

fromResponseToHttpResponse :: Http.Request -> Http.Response a -> HttpResponse a
fromResponseToHttpResponse request response =
  HttpResponse { httpResponseRequestHeaders  = requestHeaders
               , httpResponseRequestBody     = requestBody
               , httpResponseResponseStatus  = Http.getResponseStatus response
               , httpResponseResponseHeaders = Http.getResponseHeaders response
               , httpResponseResponseBody = Http.getResponseBody response
               }
  where
    requestHeaders :: [(String, String)]
    requestHeaders =
      Http.requestHeaders request <&> \(headerName, headerValue) ->
        ( CI.original headerName & BSU.toString
        , BSU.toString headerValue
        )

    requestBody :: String
    requestBody =
      case Http.requestBody request of
         Http.RequestBodyLBS bs -> BLU.toString bs
         Http.RequestBodyBS bs  -> BSU.toString bs
         _                      -> "body cannot be read"

-- * templated request computation input


data TemplatedRequestComputationInput = TemplatedRequestComputationInput
    { _templatedRequestComputationInputMethod  :: Method
    , _templatedRequestComputationInputHeaders :: [([Template], [Template])]
    , _templatedRequestComputationInputUrl     :: [Template]
    , _templatedRequestComputationInputBody    :: [Template]
    }
    deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON TemplatedRequestComputationInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON TemplatedRequestComputationInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * request computation input


data RequestComputationInput = RequestComputationInput
    { _requestComputationInputMethod  :: Method
    , _requestComputationInputHeaders :: [(String, String)]
    , _requestComputationInputUrl     :: String
    , _requestComputationInputBody    :: String
    }
    deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON RequestComputationInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON RequestComputationInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * request computation


data RequestComputation = RequestComputation
    { _requestComputationRequestHeaders     :: [(String, String)]
    , _requestComputationRequestBody        :: String
    , _requestComputationResponseStatusCode :: Int
    , _requestComputationResponseHeaders    :: [(String, String)]
    , _requestComputationResponseBody       :: String
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON RequestComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON RequestComputation where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * request computation output


type RequestComputationOutput
  = Either HttpException RequestComputation


-- * http exception


data HttpException = InvalidUrlException String String
    | TooManyRedirects
    | OverlongHeaders
    | ResponseTimeout
    | ConnectionTimeout
    | ConnectionFailure String
    | InvalidStatusLine
    | InvalidHeader
    | InvalidRequestHeader
    | InternalException String
    | ProxyConnectException
    | NoResponseDataReceived
    | WrongRequestBodyStreamSize
    | ResponseBodyTooShort
    | InvalidChunkHeaders
    | IncompleteHeaders
    | InvalidDestinationHost
    | HttpZlibException
    | InvalidProxyEnvironmentVariable
    | ConnectionClosed
    | InvalidProxySettings
    | UnknownException
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON HttpException where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                             , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                             }

instance Aeson.FromJSON HttpException where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                                , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                                }
