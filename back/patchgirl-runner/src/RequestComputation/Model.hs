{-# LANGUAGE DeriveGeneric #-}

module RequestComputation.Model ( RequestComputationInput(..)
                                , TemplatedRequestComputationInput(..)
                                , RequestComputation(..)
                                , RequestComputationOutput
                                , HttpResponse(..)
                                , fromResponseToHttpResponse
                                , HttpException(..)
                                ) where


import qualified Data.Aeson          as Aeson
import           GHC.Generics        (Generic)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types  as Http
import           PatchGirl.Client

import           Interpolator


-- * http response


data HttpResponse body = HttpResponse { httpResponseStatus  :: Http.Status
                                      , httpResponseHeaders :: Http.ResponseHeaders
                                      , httpResponseBody    :: body
                                      } deriving (Show)

fromResponseToHttpResponse :: Http.Response a -> HttpResponse a
fromResponseToHttpResponse response =
  HttpResponse { httpResponseStatus  = Http.getResponseStatus response
               , httpResponseHeaders = Http.getResponseHeaders response
               , httpResponseBody = Http.getResponseBody response
               }


-- * templated request computation input


data TemplatedRequestComputationInput
  = TemplatedRequestComputationInput { _templatedRequestComputationInputMethod  :: Method
                                     , _templatedRequestComputationInputHeaders :: [( [Template], [Template])]
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


data RequestComputationInput
  = RequestComputationInput { _requestComputationInputMethod  :: Method
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


data RequestComputation
  = RequestComputation { _requestComputationStatusCode :: Int
                       , _requestComputationHeaders    :: [(String, String)]
                       , _requestComputationBody       :: String
                       }
  deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON RequestComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON RequestComputation where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * request computation result


type RequestComputationOutput
  = Either HttpException RequestComputation


-- * http exception


data HttpException
  = InvalidUrlException String String
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
