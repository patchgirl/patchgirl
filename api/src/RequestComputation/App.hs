{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module RequestComputation.App where


import           Control.Monad.Except      (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans       (liftIO)
import           Data.Aeson                (FromJSON, ToJSON, genericParseJSON,
                                            parseJSON)
import           Data.Aeson                (ToJSON (..), genericToJSON)
import           Data.Aeson.Types          (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.CaseInsensitive      as CI
import           GHC.Generics              (Generic)
import           Http
import qualified Network.HTTP.Simple       as Http
import qualified Network.HTTP.Types.Header as Http
import           PatchGirl
import           Servant.Server            (ServerError)

-- * model


-- ** request computation input

data RequestComputationInput
  = RequestComputationInput { _requestComputationInputMethod  :: Method
                            , _requestComputationInputHeaders :: [(String, String)]
                            , _requestComputationInputScheme  :: Scheme
                            , _requestComputationInputUrl     :: String
                            , _requestComputationInputBody    :: String
                            }
  deriving (Eq, Show, Read, Generic)

instance ToJSON RequestComputationInput where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestComputationInput where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

mkHeader :: (String, String) -> Http.Header
mkHeader (headerName, headerValue) =
  (CI.mk $ BSU.fromString headerName, BSU.fromString headerValue)


-- ** request computation result


data RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotRequestComputationOutput RequestComputationOutput
    deriving (Generic)

instance ToJSON RequestComputationResult


-- ** request computation output


data RequestComputationOutput
  = RequestComputationOutput { statusCode :: Int
                             , headers    :: [(String, String)]
                             , body       :: String
                             }
  deriving (Eq, Show, Read, Generic)

instance ToJSON RequestComputationOutput where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestComputationOutput where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * handler


runRequestComputationHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => RequestComputationInput
  -> m RequestComputationResult
runRequestComputationHandler requestComputationInput = do
  response <- runRequest requestComputationInput
  return $ GotRequestComputationOutput $ createRequestComputationOutput response



runRequest
  :: MonadIO m
  => RequestComputationInput
  -> m (Http.Response BSU.ByteString)
runRequest (RequestComputationInput { _requestComputationInputMethod
                                    , _requestComputationInputHeaders
                                    , _requestComputationInputScheme
                                    , _requestComputationInputUrl
                                    , _requestComputationInputBody
                                    }) = do
  parsedRequest <- liftIO $ Http.parseRequest _requestComputationInputUrl
  let request
        = Http.setRequestMethod (BSU.fromString $ methodToString _requestComputationInputMethod)
        $ Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
        $ setPortAndSecure
        $ Http.setRequestBodyLBS (BLU.fromString _requestComputationInputBody)
        $ parsedRequest
  liftIO $ Http.httpBS request
  where
    setPortAndSecure :: Http.Request -> Http.Request
    setPortAndSecure =
      case _requestComputationInputScheme of
        Http ->
          (Http.setRequestSecure True) . (Http.setRequestPort 80)

        Https ->
          (Http.setRequestSecure False) . (Http.setRequestPort 443)

createRequestComputationOutput :: Http.Response BSU.ByteString -> RequestComputationOutput
createRequestComputationOutput response =
  RequestComputationOutput { statusCode = Http.getResponseStatusCode response
                           , headers    = parseResponseHeaders response
                           , body       = BSU.toString $ Http.getResponseBody response
                           }

parseResponseHeaders :: Http.Response BSU.ByteString -> [(String,String)]
parseResponseHeaders response =
  map convert (Http.getResponseHeaders response)
  where
    convert :: (Http.HeaderName, BSU.ByteString) -> (String, String)
    convert (headerKey, headerValue) =
      ( BSU.toString $ CI.original headerKey
      , BSU.toString headerValue
      )
