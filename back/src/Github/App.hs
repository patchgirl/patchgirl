{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Github.App where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          genericParseJSON, genericToJSON,
                                          parseJSON)
import qualified Data.Aeson              as Aeson
import           Data.Aeson.Types        (defaultOptions, fieldLabelModifier)
import           Data.Aeson.Types        ((.:), (.=))
import qualified Data.ByteString.Char8   as B8
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import           Debug.Trace
import           GHC.Generics            (Generic)
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Simple     as HTTP
import           Servant
import qualified Servant.Client          as Client


-- * github sign in


data SignInWithGithub
  = SignInWithGithub { _signInWithGithubCode :: String
                     }
  deriving (Eq, Show, Read, Generic)

instance ToJSON SignInWithGithub where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SignInWithGithub where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * github access token


-- ** model


-- *** input


data GithubOAuthCredentials
  = GithubOAuthCredentials { _githubOAuthCredentialsClientId     :: String
                           , _githubOAuthCredentialsClientSecret :: String
                           , _githubOAuthCredentialsCode         :: String
                           } deriving (Show)

instance ToJSON GithubOAuthCredentials where
    toJSON GithubOAuthCredentials {..} =
        Aeson.object [ "client_id" .= _githubOAuthCredentialsClientId
                     , "client_secret" .= _githubOAuthCredentialsClientSecret
                     , "code" .= _githubOAuthCredentialsCode
                     ]


-- *** output


data GithubAccessToken
  = GithubAccessToken { _githubAccessTokenAccessToken :: String
                      , _githubAccessTokenTokenType   :: String
                      , _githubAccessTokenScope       :: String
                      } deriving (Show)

instance FromJSON GithubAccessToken where
  parseJSON = Aeson.withObject "GithubProfile" $ \o -> do
    _githubAccessTokenAccessToken <- o .: "access_token"
    _githubAccessTokenTokenType <- o .: "token_type"
    _githubAccessTokenScope <- o .: "scope"
    return $ GithubAccessToken{..}


-- ** api


type GithubAccessTokenApi =
  ReqBody '[JSON] GithubOAuthCredentials :>
  Post '[JSON] GithubAccessToken


-- ** handler


getGithubAccessTokenClient :: GithubOAuthCredentials -> IO (Maybe String)
getGithubAccessTokenClient githubOAuthCredentials = do
  manager <- HTTP.newManager TLS.tlsManagerSettings
  Client.runClientM (githubProfileClient githubOAuthCredentials) (clientEnv manager) >>= return . \case
    Left err -> Nothing
    Right GithubAccessToken {..} -> Just _githubAccessTokenAccessToken
  where
    baseUrl =
      Client.BaseUrl { baseUrlScheme = Client.Https
                     , baseUrlHost = "github.com"
                     , baseUrlPort = 443
                     , baseUrlPath = "login/oauth/access_token"
                     }
    githubProfileClient =
      Client.client (Proxy :: Proxy GithubAccessTokenApi)
    clientEnv manager =
      Client.mkClientEnv manager baseUrl


-- * github profile


-- ** model


data GithubProfile
  = GithubProfile { _githubProfileId        :: Int
                  , _githubProfileAvatarUrl :: String
                  , _githubProfileEmail     :: String
                  } deriving (Show)

instance FromJSON GithubProfile where
  parseJSON = Aeson.withObject "GithubProfile" $ \o -> do
    _githubProfileId <- o .: "id"
    _githubProfileAvatarUrl <- o .: "avatar_url"
    _githubProfileEmail <- o .: "email"
    return $ GithubProfile{..}


-- ** api


type GithubProfileApi =
  Header "Authorization" String :>
  Header "User-Agent" String :>
  Get '[JSON] GithubProfile


-- ** handler


getGithubProfileClient :: String -> IO (Maybe GithubProfile)
getGithubProfileClient accessToken = do
  manager <- HTTP.newManager TLS.tlsManagerSettings
  Client.runClientM githubProfileClient (clientEnv manager) >>= return . \case
    Left _ ->  Nothing
    Right profile -> Just profile
  where
    baseUrl =
      Client.BaseUrl { baseUrlScheme = Client.Https
                     , baseUrlHost = "api.github.com"
                     , baseUrlPort = 443
                     , baseUrlPath = "user"
                     }
    githubProfileClient =
      Client.client (Proxy :: Proxy GithubProfileApi) (Just ("token " <> accessToken)) (Just "servant-user-agent")
    clientEnv manager =
      Client.mkClientEnv manager baseUrl
