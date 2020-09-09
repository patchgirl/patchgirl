{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module PatchGirl.Api( WebApi
                    , webApiServer
                    , RequestCollectionApi
                    , PgCollectionApi
                    , ScenarioCollectionApi
                    , EnvironmentApi
                    , ScenarioNodeApi
                    , ScenarioFileApi
                    , ScenarioFolderApi
                    , SceneActorApi
                    , RequestNodeApi
                    , RequestFileApi
                    , RequestFolderApi
                    , PgNodeApi
                    , PgFileApi
                    , PgFolderApi
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
import           Environment.App
import           Environment.Model
import           Github.App
import           Health.App
import           PatchGirl.Model
import           PgCollection.App
import           PgCollection.Model
import           PgNode.App
import           PgNode.Model
import           RequestCollection.App
import           RequestCollection.Model
import           RequestNode.App
import           RequestNode.Model
import           ScenarioCollection.App
import           ScenarioCollection.Model
import           ScenarioNode.App
import           ScenarioNode.Model
import           Servant.Auth.Server.Internal.ThrowAll (ThrowAll)
import           Session.App
import           Session.Model
import           Test


type WebApi auths =
  RestApi auths :<|>
  TestApi :<|>
  AssetApi


webApiServer :: CookieSettings -> JWTSettings -> ServerT (WebApi a) AppM
webApiServer cookieSettings jwtSettings =
  restApiServer cookieSettings jwtSettings :<|>
  testApiServer :<|>
  assetApiServer


-- * rest


type RestApi auths =
  RequestCollectionApi auths :<|>
  PgCollectionApi auths :<|>
  ScenarioCollectionApi auths :<|>
  EnvironmentApi auths :<|>
  ScenarioNodeApi auths :<|>
  ScenarioFileApi auths :<|>
  ScenarioFolderApi auths :<|>
  SceneActorApi auths :<|>
  RequestNodeApi auths :<|>
  RequestFileApi auths :<|>
  RequestFolderApi auths :<|>
  PgNodeApi auths :<|>
  PgFileApi auths :<|>
  PgFolderApi auths :<|>
  SessionApi :<|>
  PSessionApi auths :<|>
  AccountApi  :<|>
  HealthApi

restApiServer :: CookieSettings -> JWTSettings -> ServerT (RestApi a) AppM
restApiServer cookieSettings jwtSettings =
   requestCollectionApiServer
  :<|> pgCollectionApiServer
  :<|> scenarioCollectionApiServer
  :<|> environmentApiServer
  :<|> scenarioNodeApiServer
  :<|> scenarioFileApiServer
  :<|> scenarioFolderApiServer
  :<|> sceneApiServer
  :<|> requestNodeApiServer
  :<|> requestFileApiServer
  :<|> requestFolderApiServer
  :<|> pgNodeApiServer
  :<|> pgFileApiServer
  :<|> pgFolderApiServer
  :<|> sessionApiServer cookieSettings jwtSettings
  :<|> pSessionApiServer cookieSettings jwtSettings
  :<|> accountApiServer
  :<|> healthApiServer


-- * request collection


type RequestCollectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Get '[JSON] RequestCollection)

requestCollectionApiServer :: (AuthResult CookieSession -> AppM RequestCollection)
requestCollectionApiServer =
  authorizeWithAccountId getRequestCollectionHandler


-- * pg collection


type PgCollectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Get '[JSON] PgCollection)

pgCollectionApiServer :: (AuthResult CookieSession -> AppM PgCollection)
pgCollectionApiServer =
  authorizeWithAccountId getPgCollectionHandler


-- * scenario collection


type ScenarioCollectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Get '[JSON] ScenarioCollection)

scenarioCollectionApiServer :: (AuthResult CookieSession -> AppM ScenarioCollection)
scenarioCollectionApiServer =
  authorizeWithAccountId getScenarioCollectionHandler


-- * environment


type EnvironmentApi auths =
  Flat (Auth auths CookieSession :> "api" :> "environment" :> (
    ReqBody '[JSON] NewEnvironment :> Post '[JSON] UUID :<|> -- createEnvironment
    Get '[JSON] [Environment] :<|> -- getEnvironments
    Capture "environmentId" UUID :> (
      ReqBody '[JSON] UpdateEnvironment :> Put '[JSON] () :<|> -- updateEnvironment
      Delete '[JSON] () :<|>  -- deleteEnvironment
      "keyValue" :> (
        ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] [KeyValue] :<|> -- updateKeyValues
        Capture "keyValueId" UUID :> Delete '[JSON] () -- deleteKeyValues
      )
    )
  ))

environmentApiServer
  :: (AuthResult CookieSession -> NewEnvironment -> AppM UUID)
  :<|> ((AuthResult CookieSession -> AppM [Environment])
  :<|> ((AuthResult CookieSession -> UUID -> UpdateEnvironment -> AppM ())
  :<|> ((AuthResult CookieSession -> UUID -> AppM ())
  :<|> ((AuthResult CookieSession -> UUID -> [NewKeyValue] -> AppM [KeyValue])
  :<|> (AuthResult CookieSession -> UUID -> UUID -> AppM ())))))
environmentApiServer =
  authorizeWithAccountId createEnvironmentHandler
  :<|> authorizeWithAccountId getEnvironmentsHandler
  :<|> authorizeWithAccountId updateEnvironmentHandler
  :<|> authorizeWithAccountId deleteEnvironmentHandler
  :<|> authorizeWithAccountId updateKeyValuesHandler
  :<|> authorizeWithAccountId deleteKeyValueHandler


-- * scenario node


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


-- * scenario file


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


-- * scenario folder


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


-- * scene


type SceneActorApi auths =
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


-- * request node


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


-- * request file api


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


-- * request folder api


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


-- * pg node


type PgNodeApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" UUID :> "pgNode" :> Capture "pgNodeId" UUID :> (
    -- rename pg node
    ReqBody '[JSON] UpdatePgNode :> Put '[JSON] () :<|>
    -- delete pg node
    Delete '[JSON] ()
  ))

pgNodeApiServer
  :: (AuthResult CookieSession -> UUID -> UUID -> UpdatePgNode -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UUID -> AppM ())
pgNodeApiServer =
  authorizeWithAccountId updatePgNodeHandler
  :<|> authorizeWithAccountId deletePgNodeHandler


-- * pg file api


type PgFileApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" UUID :> (
    "pgFile" :> (
      -- createPgFile
      ReqBody '[JSON] NewPgFile :> Post '[JSON] ()
    ) :<|> "rootPgFile" :> (
      -- create root pg file
      ReqBody '[JSON] NewRootPgFile :> Post '[JSON] ()
    ) :<|> Capture "pgNodeId" UUID :> ReqBody '[JSON] UpdatePgFile :> Put '[JSON] ()
  ))

pgFileApiServer
  :: (AuthResult CookieSession -> UUID -> NewPgFile -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> NewRootPgFile -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> UUID -> UpdatePgFile -> AppM ())
pgFileApiServer =
  authorizeWithAccountId createPgFileHandler
  :<|> authorizeWithAccountId createRootPgFileHandler
  :<|> authorizeWithAccountId updatePgFileHandler


-- * pg folder api


type PgFolderApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" UUID :> (
    "pgFolder" :> (
      -- create pg folder
      ReqBody '[JSON] NewPgFolder :> Post '[JSON] ()
    ) :<|> "rootPgFolder" :> (
      -- create root pg folder
      ReqBody '[JSON] NewRootPgFolder :> Post '[JSON] ()
    )
  ))

pgFolderApiServer
  :: (AuthResult CookieSession -> UUID -> NewPgFolder -> AppM ())
  :<|> (AuthResult CookieSession -> UUID -> NewRootPgFolder -> AppM ())
pgFolderApiServer =
  authorizeWithAccountId createPgFolderHandler
  :<|> authorizeWithAccountId createRootPgFolderHandler


-- * session


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


-- * whoami


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


-- * account

type AccountApi =
  "api" :> "account" :> (
    "resetVisitorAccount" :> Get '[JSON] ()
  )

accountApiServer :: ServerT AccountApi AppM
accountApiServer =
  resetVisitorAccountHandler


-- * health


type HealthApi =
  "api" :> "health" :> Get '[JSON] AppHealth

healthApiServer :: ServerT HealthApi AppM
healthApiServer =
  getAppHealthHandler


-- * other


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
