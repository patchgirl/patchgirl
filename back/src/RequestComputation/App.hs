{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module RequestComputation.App where


import           Control.Monad.Except      (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans       (liftIO)
import           Data.Aeson                (FromJSON, ToJSON (..),
                                            genericParseJSON, genericToJSON,
                                            parseJSON)
import           Data.Aeson.Types          (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.CaseInsensitive      as CI
import           GHC.Generics              (Generic)
import           Http
import qualified Network.HTTP.Client.TLS   as Tls
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

instance ToJSON RequestComputationResult where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


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


-- * handler


runRequestComputationHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> RequestComputationInput
  -> m RequestComputationResult
runRequestComputationHandler _ requestComputationInput = do
  response <- runRequest requestComputationInput
  liftIO $ print response
  return $ GotRequestComputationOutput $ createRequestComputationOutput response



runRequest
  :: MonadIO m
  => RequestComputationInput
  -> m (Http.Response BSU.ByteString)
runRequest RequestComputationInput { _requestComputationInputMethod
                                   , _requestComputationInputHeaders
                                   , _requestComputationInputScheme
                                   , _requestComputationInputUrl
                                   , _requestComputationInputBody
                                   } = do
  let url = schemeToString _requestComputationInputScheme <> "://" <> _requestComputationInputUrl
  manager <- Tls.newTlsManager
  liftIO $ Tls.setGlobalManager manager
  liftIO $ print url
  parsedRequest <- liftIO $ Http.parseRequest url
  liftIO $ print $ show parsedRequest
  let request
        = Http.setRequestMethod (BSU.fromString $ methodToString _requestComputationInputMethod)
        $ Http.setRequestHeaders (map mkHeader _requestComputationInputHeaders)
        $ setPortAndSecure
        $ Http.setRequestBodyLBS (BLU.fromString _requestComputationInputBody)
        $ Http.setRequestManager manager parsedRequest
  liftIO $ Http.httpBS request
  where
    setPortAndSecure :: Http.Request -> Http.Request
    setPortAndSecure =
      case _requestComputationInputScheme of
        Http ->
          Http.setRequestSecure False . Http.setRequestPort 80

        Https ->
          Http.setRequestSecure True . Http.setRequestPort 443


createRequestComputationOutput :: Http.Response BSU.ByteString -> RequestComputationOutput
createRequestComputationOutput response =
  RequestComputationOutput
    { _requestComputationOutputStatusCode = Http.getResponseStatusCode response
    , _requestComputationOutputHeaders    = parseResponseHeaders response
    , _requestComputationOutputBody       = BSU.toString $ Http.getResponseBody response
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
