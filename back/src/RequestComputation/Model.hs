{-# LANGUAGE DeriveGeneric #-}

module RequestComputation.Model ( RequestComputationInput(..)
                                , RequestComputationOutput(..)
                                , RequestComputationResult(..)
                                , HttpResponse(..)
                                , fromResponseToHttpResponse
                                ) where


import           Data.Aeson                 (FromJSON, ToJSON (..),
                                             genericParseJSON, genericToJSON,
                                             parseJSON)
import           Data.Aeson.Types           (defaultOptions, fieldLabelModifier)
import           GHC.Generics               (Generic)
import           Http
import qualified Network.HTTP.Client        as Http
import qualified Network.HTTP.Simple        as Http
import qualified Network.HTTP.Types.Header  as Http
import qualified Network.HTTP.Types.Status  as Http
import qualified Network.HTTP.Types.Version as Http


-- * model


-- ** response


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


-- ** request computation input


data RequestComputationInput
  = RequestComputationInput { _requestComputationInputMethod  :: Method
                            , _requestComputationInputHeaders :: [(String, String)]
                            , _requestComputationInputScheme  :: Scheme
                            , _requestComputationInputUrl     :: String
                            , _requestComputationInputBody    :: String
                            }
  deriving (Eq, Show, Read, Generic, Ord)

instance ToJSON RequestComputationInput where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestComputationInput where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- ** request computation result


data RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotRequestComputationOutput RequestComputationOutput
    deriving (Eq, Show, Generic)

instance ToJSON RequestComputationResult where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestComputationResult where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- ** request computation output


data RequestComputationOutput
  = RequestComputationOutput { _requestComputationOutputStatusCode :: Int
                             , _requestComputationOutputHeaders    :: [(String, String)]
                             , _requestComputationOutputBody       :: String
                             }
  deriving (Eq, Show, Read, Generic)

instance ToJSON RequestComputationOutput where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestComputationOutput where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
