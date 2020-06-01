{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module PatchGirl.Api( CombinedApi
                    , combinedApiServer
                    , RequestCollectionApi
                    , ScenarioCollectionApi
                    , EnvironmentApi
                    , ScenarioNodeApi
                    , ScenarioFileApi
                    , ScenarioFolderApi
                    , SceneApi
                    , RequestNodeApi
                    , RequestFileApi
                    , RequestFolderApi
                    , RequestComputationApi
                    , ScenarioComputationApi
                    , SessionApi
                    , PSessionApi
                    , AccountApi
                    , RestApi
                    , TestApi
                    ) where


import           Data.UUID
import           Servant                               hiding (BadPassword,
                                                        NoSuchUser)
import           Servant.API.Flatten                   (Flat)
import           Servant.Auth.Server                   (Auth, AuthResult (..),
                                                        CookieSettings,
                                                        JWTSettings, SetCookie,
                                                        throwAll)

import           Account.App
import           Env
import           Environment.App
import           Environment.Model
import           FrontConfig.App
import           Github.App
import           Health.App
import           Interpolator
import           PatchGirl.Model
import           RequestCollection.App
import           RequestCollection.Model
import           RequestComputation.App
import           RequestComputation.Model
import           RequestNode.App
import           RequestNode.Model
import           ScenarioCollection.App
import           ScenarioCollection.Model
import           ScenarioComputation.App
import           ScenarioComputation.Model
import           ScenarioNode.App
import           ScenarioNode.Model
import           Servant.Auth.Server.Internal.ThrowAll (ThrowAll)
import           Session.App
import           Session.Model
import           Test


-- * api


-- ** combined


type CombinedApi auths =
  RestApi auths :<|>
  TestApi :<|>
  AssetApi

combinedApiServer :: CookieSettings -> JWTSettings -> ServerT (CombinedApi a) AppM
combinedApiServer cookieSettings jwtSettings =
  restApiServer cookieSettings jwtSettings :<|>
  testApiServer :<|>
  assetApiServer


-- ** rest


type RestApi auths =
  RequestCollectionApi auths :<|>
  ScenarioCollectionApi auths :<|>
  EnvironmentApi auths :<|>
  ScenarioNodeApi auths :<|>
  ScenarioFileApi auths :<|>
  ScenarioFolderApi auths :<|>
  SceneApi auths :<|>
  RequestNodeApi auths :<|>
  RequestFileApi auths :<|>
  RequestFolderApi auths :<|>
  RequestComputationApi :<|>
  ScenarioComputationApi :<|>
  SessionApi :<|>
  PSessionApi auths :<|>
  AccountApi  :<|>
  HealthApi :<|>
  ConfigApi

restApiServer :: CookieSettings -> JWTSettings -> ServerT (RestApi a) AppM
restApiServer cookieSettings jwtSettings =
   requestCollectionApiServer
  :<|> scenarioCollectionApiServer
  :<|> environmentApiServer
  :<|> scenarioNodeApiServer
  :<|> scenarioFileApiServer
  :<|> scenarioFolderApiServer
  :<|> sceneApiServer
  :<|> requestNodeApiServer
  :<|> requestFileApiServer
  :<|> requestFolderApiServer
  :<|> requestComputationApiServer
  :<|> scenarioComputationApiServer
  :<|> sessionApiServer cookieSettings jwtSettings
  :<|> pSessionApiServer cookieSettings jwtSettings
  :<|> accountApiServer
  :<|> healthApiServer
  :<|> configApiServer


-- ** request collection


type RequestCollectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Get '[JSON] RequestCollection)

requestCollectionApiServer :: (AuthResult CookieSession -> AppM RequestCollection)
requestCollectionApiServer =
  authorizeWithAccountId getRequestCollectionHandler


-- ** scenario collection


type ScenarioCollectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Get '[JSON] ScenarioCollection)

scenarioCollectionApiServer :: (AuthResult CookieSession -> AppM ScenarioCollection)
scenarioCollectionApiServer =
  authorizeWithAccountId getScenarioCollectionHandler


-- ** environment


type EnvironmentApi auths =
  Flat (Auth auths CookieSession :> "api" :> "environment" :> (
    ReqBody '[JSON] NewEnvironment :> Post '[JSON] Int :<|> -- createEnvironment
    Get '[JSON] [Environment] :<|> -- getEnvironments
    Capture "environmentId" Int :> (
      ReqBody '[JSON] UpdateEnvironment :> Put '[JSON] () :<|> -- updateEnvironment
      Delete '[JSON] () :<|>  -- deleteEnvironment
      "keyValue" :> (
        ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] [KeyValue] :<|> -- updateKeyValues
        Capture "keyValueId" Int :> Delete '[JSON] () -- deleteKeyValues
      )
    )
  ))

environmentApiServer
  :: (AuthResult CookieSession -> NewEnvironment -> AppM Int)
  :<|> ((AuthResult CookieSession -> AppM [Environment])
  :<|> ((AuthResult CookieSession -> Int -> UpdateEnvironment -> AppM ())
  :<|> ((AuthResult CookieSession -> Int -> AppM ())
  :<|> ((AuthResult CookieSession -> Int -> [NewKeyValue] -> AppM [KeyValue])
  :<|> (AuthResult CookieSession -> Int -> Int -> AppM ())))))
environmentApiServer =
  authorizeWithAccountId createEnvironmentHandler
  :<|> authorizeWithAccountId getEnvironmentsHandler
  :<|> authorizeWithAccountId updateEnvironmentHandler
  :<|> authorizeWithAccountId deleteEnvironmentHandler
  :<|> authorizeWithAccountId updateKeyValuesHandler
  :<|> authorizeWithAccountId deleteKeyValueHandler


-- ** scenario node


type ScenarioNodeApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" UUID :> "scenarioNode" :> Capture "scenarioNodeId" UUID :> (
    -- rename scenario node
    ReqBody '[JSON] UpdateScenarioNode :> Put '[JSON] () :<|>
    -- delete scenario node
    Delete '[JSON] ()
  ))

scenarioNodeApiServer
  :: (AuthResult CookieSession -> UUID -> UUID -> UpdateScenarioNode -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UUID -> AppM ())
scenarioNodeApiServer =
  authorizeWithAccountId updateScenarioNodeHandler
  :<|> authorizeWithAccountId deleteScenarioNodeHandler

-- ** scenario file


type ScenarioFileApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" UUID :> (
    "scenarioFile" :> (
      -- createScenarioFile
      ReqBody '[JSON] NewScenarioFile :> Post '[JSON] () :<|>
      -- updateScenarioFile
      ReqBody '[JSON] UpdateScenarioFile :> Put '[JSON] ()
    ) :<|> "rootScenarioFile" :> (
        -- create root scenario file
        ReqBody '[JSON] NewRootScenarioFile :> Post '[JSON] ()
    )
  ))

scenarioFileApiServer
  :: (AuthResult CookieSession -> UUID -> NewScenarioFile -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UpdateScenarioFile -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> NewRootScenarioFile -> AppM ())
scenarioFileApiServer =
  authorizeWithAccountId createScenarioFileHandler
  :<|> authorizeWithAccountId updateScenarioFileHandler
  :<|> authorizeWithAccountId createRootScenarioFileHandler


-- ** scenario folder


type ScenarioFolderApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" UUID :> (
    "scenarioFolder" :> (
      -- create scenario folder
      ReqBody '[JSON] NewScenarioFolder :> Post '[JSON] ()
    ) :<|>
      "rootScenarioFolder" :> (
        -- create root scenario folder
        ReqBody '[JSON] NewRootScenarioFolder :> Post '[JSON] ()
      )
    ))

scenarioFolderApiServer
  :: (AuthResult CookieSession -> UUID -> NewScenarioFolder -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> NewRootScenarioFolder -> AppM ())
scenarioFolderApiServer =
  authorizeWithAccountId createScenarioFolderHandler
  :<|> authorizeWithAccountId createRootScenarioFolderHandler


-- ** scene


type SceneApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioNode" :> Capture "scenarioNodeId" UUID :> (
    "scene" :> (
      -- create scene
      ReqBody '[JSON] NewScene :> Post '[JSON] () :<|>
      -- delete scene
      Capture "sceneId" UUID :> Delete '[JSON] () :<|>
      -- update scene
      Capture "sceneId" UUID :> ReqBody '[JSON] UpdateScene :> Put '[JSON] ()
    )
  ))

sceneApiServer
  :: (AuthResult CookieSession -> UUID -> NewScene -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UUID -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UUID -> UpdateScene -> AppM ())
sceneApiServer =
  authorizeWithAccountId createSceneHandler
  :<|> authorizeWithAccountId deleteSceneHandler
  :<|> authorizeWithAccountId updateSceneHandler


-- ** request node


type RequestNodeApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" UUID :> (
    -- rename request node
    ReqBody '[JSON] UpdateRequestNode :> Put '[JSON] () :<|>
    -- delete request node
    Delete '[JSON] ()
  ))

requestNodeApiServer
  :: (AuthResult CookieSession -> Int -> UUID -> UpdateRequestNode -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> UUID -> AppM ())
requestNodeApiServer =
  authorizeWithAccountId updateRequestNodeHandler
  :<|> authorizeWithAccountId deleteRequestNodeHandler


-- ** request file api


type RequestFileApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> (
    "requestFile" :> (
      -- createRequestFile
      ReqBody '[JSON] NewRequestFile :> Post '[JSON] ()
    ) :<|> "rootRequestFile" :> (
      -- create root request file
      ReqBody '[JSON] NewRootRequestFile :> Post '[JSON] ()
    ) :<|> Capture "requestNodeId" UUID :> ReqBody '[JSON] UpdateRequestFile :> Put '[JSON] ()
  ))

requestFileApiServer
  :: (AuthResult CookieSession -> Int -> NewRequestFile -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> NewRootRequestFile -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> UUID -> UpdateRequestFile -> AppM ())
requestFileApiServer =
  authorizeWithAccountId createRequestFileHandler
  :<|> authorizeWithAccountId createRootRequestFileHandler
  :<|> authorizeWithAccountId updateRequestFileHandler


-- ** request folder api


type RequestFolderApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> (
    "requestFolder" :> (
      -- create request folder
      ReqBody '[JSON] NewRequestFolder :> Post '[JSON] ()
    ) :<|> "rootRequestFolder" :> (
      -- create root request folder
      ReqBody '[JSON] NewRootRequestFolder :> Post '[JSON] ()
    )
  ))

requestFolderApiServer
  :: (AuthResult CookieSession -> Int -> NewRequestFolder -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> NewRootRequestFolder -> AppM ())
requestFolderApiServer =
  authorizeWithAccountId createRequestFolderHandler
  :<|> authorizeWithAccountId createRootRequestFolderHandler


-- ** request computation


type RequestComputationApi =
  "api" :> "runner" :> "requestComputation" :> (
    ReqBody '[JSON] (TemplatedRequestComputationInput, EnvironmentVars) :> Post '[JSON] RequestComputationResult
  )

requestComputationApiServer :: (TemplatedRequestComputationInput, EnvironmentVars) -> AppM RequestComputationResult
requestComputationApiServer =
  runRequestComputationHandler


-- ** scenario computation


type ScenarioComputationApi =
  "api" :> "runner" :> "scenarioComputation" :> (
    ReqBody '[JSON] ScenarioInput :> Post '[JSON] ScenarioOutput
  )

scenarioComputationApiServer :: ScenarioInput -> AppM ScenarioOutput
scenarioComputationApiServer =
  runScenarioComputationHandler


-- ** session


type SessionApi =
  "api" :> "session" :> (
    "signInWithGithub" :> ReqBody '[JSON] SignInWithGithub :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                                     , Header "Set-Cookie" SetCookie
                                                                                     ] Session) :<|>
    "signout" :> Delete '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ] Session)
  )

sessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT SessionApi AppM
sessionApiServer cookieSettings jwtSettings  =
  signInOnGithubHandler cookieSettings jwtSettings :<|>
  deleteSessionHandler cookieSettings


-- ** whoami


type PSessionApi auths =
  Auth auths CookieSession :> WhoAmiApi


type WhoAmiApi =
  "api" :> "session" :> "whoami" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                          , Header "Set-Cookie" SetCookie
                                                          ] Session)
pSessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> AuthResult CookieSession
  -> ServerT WhoAmiApi AppM
pSessionApiServer cookieSettings jwtSettings cookieSessionAuthResult =
  whoAmIHandler cookieSettings jwtSettings cookieSessionAuthResult


-- ** account

type AccountApi =
  "api" :> "account" :> (
    "resetVisitorAccount" :> Get '[JSON] ()
  )

accountApiServer :: ServerT AccountApi AppM
accountApiServer =
  resetVisitorAccountHandler


-- ** health


type HealthApi =
  "api" :> "health" :> Get '[JSON] AppHealth

healthApiServer :: ServerT HealthApi AppM
healthApiServer =
  getAppHealthHandler


-- ** config


type ConfigApi =
  "api" :> "config" :> Get '[JSON] FrontConfig

configApiServer :: ServerT ConfigApi AppM
configApiServer =
  getConfigApiHandler


-- ** other


type TestApi =
  Flat (
    "test" :> (
      "deleteNoContent" :> DeleteNoContent '[JSON] NoContent :<|>
      "getStatusCode" :> Capture "statusCode" Int :> Get '[JSON] () :<|>
       -- create
      "users" :> ReqBody '[JSON] NewUserTest :> Post '[JSON] UserTest :<|>
      -- delete
      "users" :> Capture "userId" Int :> Delete '[JSON] () :<|>
      -- show
      "users" :> Capture "userId" Int :> Get '[JSON] UserTest :<|>
      -- update
      "users" :> Capture "userId" Int :> ReqBody '[JSON] UpdateUserTest :> Put '[JSON] UserTest :<|>
      -- list
      "users" :> Get '[JSON] [UserTest]
    )
  )

testApiServer :: ServerT TestApi AppM
testApiServer =
  deleteNoContentHandler :<|>
  getStatusCodeHandler :<|>
  createUserHandler :<|>
  deleteUserHandler :<|>
  showUserHandler :<|>
  updateUserHandler :<|>
  listUserHandler

type AssetApi =
  "public" :> Raw


-- * util


authorizeWithAccountId
  :: Servant.Auth.Server.Internal.ThrowAll.ThrowAll p
  => (UUID -> p) -> AuthResult CookieSession -> p
authorizeWithAccountId f = \case
  BadPassword ->
    throwAll err402

  NoSuchUser ->
    throwAll err403

  Indefinite ->
    throwAll err405

  Authenticated cookieSession ->
    f (_cookieAccountId cookieSession)

assetApiServer :: ServerT AssetApi AppM
assetApiServer =
  serveDirectoryWebApp "../public"
