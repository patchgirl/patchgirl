{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module PatchGirl.Web.Api( WebApi
                        , webApiServer
                        , RequestCollectionApi
                        , PgCollectionApi
                        , ScenarioCollectionApi
                        , EnvironmentApi
                        , ConnectionApi
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


import           Servant                                hiding (BadPassword,
                                                         NoSuchUser)
import           Servant.API.Flatten                    (Flat)
import           Servant.Auth.Server                    (Auth, AuthResult (..),
                                                         CookieSettings,
                                                         JWTSettings, SetCookie,
                                                         throwAll)
import           Servant.Auth.Server.Internal.ThrowAll  (ThrowAll)

import           PatchGirl.Web.Account.App
import           PatchGirl.Web.Connection.App
import           PatchGirl.Web.Connection.Model
import           PatchGirl.Web.Environment.App
import           PatchGirl.Web.Environment.Model
import           PatchGirl.Web.Github.App
import           PatchGirl.Web.Health.App
import           PatchGirl.Web.Id
import           PatchGirl.Web.Model
import           PatchGirl.Web.PgCollection.App
import           PatchGirl.Web.PgCollection.Model
import           PatchGirl.Web.PgNode.App
import           PatchGirl.Web.PgNode.Model
import           PatchGirl.Web.RequestCollection.App
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.App
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.ScenarioCollection.App
import           PatchGirl.Web.ScenarioCollection.Model
import           PatchGirl.Web.ScenarioNode.App
import           PatchGirl.Web.ScenarioNode.Model
import           PatchGirl.Web.Session.App
import           PatchGirl.Web.Session.Model
import           PatchGirl.Web.Test


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
  ConnectionApi auths :<|>
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
  :<|> connectionApiServer
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
    ReqBody '[JSON] NewEnvironment :> Post '[JSON] () :<|> -- createEnvironment
    Get '[JSON] [Environment] :<|> -- getEnvironments
    Capture "environmentId" (Id EnvId) :> (
      ReqBody '[JSON] UpdateEnvironment :> Put '[JSON] () :<|> -- updateEnvironment
      Delete '[JSON] () :<|>  -- deleteEnvironment
      "keyValue" :> (
        ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] () :<|> -- updateKeyValues
        Capture "keyValueId" (Id KeyValueId) :> Delete '[JSON] () -- deleteKeyValues
      )
    )
  ))

environmentApiServer
  :: (AuthResult CookieSession -> NewEnvironment -> AppM ())
  :<|> ((AuthResult CookieSession -> AppM [Environment])
  :<|> ((AuthResult CookieSession -> Id EnvId -> UpdateEnvironment -> AppM ())
  :<|> ((AuthResult CookieSession -> Id EnvId -> AppM ())
  :<|> ((AuthResult CookieSession -> Id EnvId -> [NewKeyValue] -> AppM ())
  :<|> (AuthResult CookieSession -> Id EnvId -> Id KeyValueId -> AppM ())))))
environmentApiServer =
  authorizeWithAccountId createEnvironmentHandler
  :<|> authorizeWithAccountId getEnvironmentsHandler
  :<|> authorizeWithAccountId updateEnvironmentHandler
  :<|> authorizeWithAccountId deleteEnvironmentHandler
  :<|> authorizeWithAccountId updateKeyValuesHandler
  :<|> authorizeWithAccountId deleteKeyValueHandler


-- * connection


type ConnectionApi auths =
  Flat (Auth auths CookieSession :> "api" :> "connection" :> (
    ReqBody '[JSON] NewConnection :> Post '[JSON] () :<|> -- createConnection
    Get '[JSON] [Connection] :<|> -- getConnections
    Capture "connectionId" (Id Con) :> (
      ReqBody '[JSON] UpdateConnection :> Put '[JSON] () :<|> -- updateConnection
      Delete '[JSON] () -- deleteConnection
    )
  ))

connectionApiServer
  :: (AuthResult CookieSession -> NewConnection -> AppM ())
  :<|> ((AuthResult CookieSession -> AppM [Connection])
  :<|> ((AuthResult CookieSession -> Id Con -> UpdateConnection -> AppM ())
  :<|> (AuthResult CookieSession -> Id Con -> AppM ())))
connectionApiServer =
  authorizeWithAccountId createConnectionHandler
  :<|> authorizeWithAccountId getConnectionsHandler
  :<|> authorizeWithAccountId updateConnectionHandler
  :<|> authorizeWithAccountId deleteConnectionHandler


-- * scenario node


type ScenarioNodeApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" (Id ScenarioCol) :> "scenarioNode" :> Capture "scenarioNodeId" (Id Scenario) :> (
    -- rename scenario node
    ReqBody '[JSON] UpdateScenarioNode :> Put '[JSON] () :<|>
    -- delete scenario node
    Delete '[JSON] ()
  ))

scenarioNodeApiServer
  :: (AuthResult CookieSession -> Id ScenarioCol -> Id Scenario -> UpdateScenarioNode -> AppM ())
  :<|> (AuthResult CookieSession -> Id ScenarioCol -> Id Scenario -> AppM ())
scenarioNodeApiServer =
  authorizeWithAccountId updateScenarioNodeHandler
  :<|> authorizeWithAccountId deleteScenarioNodeHandler


-- * scenario file


type ScenarioFileApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" (Id ScenarioCol) :> (
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
  :: (AuthResult CookieSession -> Id ScenarioCol -> NewScenarioFile -> AppM ())
  :<|> (AuthResult CookieSession -> Id ScenarioCol -> UpdateScenarioFile -> AppM ())
  :<|> (AuthResult CookieSession -> Id ScenarioCol -> NewRootScenarioFile -> AppM ())
scenarioFileApiServer =
  authorizeWithAccountId createScenarioFileHandler
  :<|> authorizeWithAccountId updateScenarioFileHandler
  :<|> authorizeWithAccountId createRootScenarioFileHandler


-- * scenario folder


type ScenarioFolderApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioCollection" :> Capture "scenarioCollectionId" (Id ScenarioCol) :> (
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
  :: (AuthResult CookieSession -> Id ScenarioCol -> NewScenarioFolder -> AppM ())
  :<|> (AuthResult CookieSession -> Id ScenarioCol -> NewRootScenarioFolder -> AppM ())
scenarioFolderApiServer =
  authorizeWithAccountId createScenarioFolderHandler
  :<|> authorizeWithAccountId createRootScenarioFolderHandler


-- * scene


type SceneActorApi auths =
  Flat (Auth auths CookieSession :> "api" :> "scenarioNode" :> Capture "scenarioNodeId" (Id Scenario) :> (
    "scene" :> (
      -- create scene
      ReqBody '[JSON] NewScene :> Post '[JSON] () :<|>
      -- delete scene
      Capture "sceneId" (Id Scene) :> Delete '[JSON] () :<|>
      -- update scene
      Capture "sceneId" (Id Scene) :> ReqBody '[JSON] UpdateScene :> Put '[JSON] ()
    )
  ))

sceneApiServer
  :: (AuthResult CookieSession -> Id Scenario -> NewScene -> AppM ())
  :<|> (AuthResult CookieSession -> Id Scenario -> Id Scene -> AppM ())
  :<|> (AuthResult CookieSession -> Id Scenario -> Id Scene -> UpdateScene -> AppM ())
sceneApiServer =
  authorizeWithAccountId createSceneHandler
  :<|> authorizeWithAccountId deleteSceneHandler
  :<|> authorizeWithAccountId updateSceneHandler


-- * request node


type RequestNodeApi auths =
  Flat (Auth auths CookieSession :> "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" (Id Request) :> (
    -- rename node
    ReqBody '[JSON] UpdateRequestNode :> Put '[JSON] () :<|>
    -- delete node
    Delete '[JSON] ()
  ))

requestNodeApiServer
  :: (AuthResult CookieSession -> Int -> Id Request -> UpdateRequestNode -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> Id Request -> AppM ())
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
    ) :<|> Capture "requestNodeId" (Id Request) :> ReqBody '[JSON] UpdateRequestFile :> Put '[JSON] ()
  ))

requestFileApiServer
  :: (AuthResult CookieSession -> Int -> NewRequestFile -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> NewRootRequestFile -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> Id Request -> UpdateRequestFile -> AppM ())
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
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" (Id PgCollection) :> "pgNode" :> Capture "pgNodeId" (Id Postgres) :> (
    -- rename pg node
    ReqBody '[JSON] UpdatePgNode :> Put '[JSON] () :<|>
    -- delete pg node
    Delete '[JSON] ()
  ))

pgNodeApiServer
  :: (AuthResult CookieSession -> Id PgCollection -> Id Postgres -> UpdatePgNode -> AppM ())
  :<|> (AuthResult CookieSession -> Id PgCollection -> Id Postgres -> AppM ())
pgNodeApiServer =
  authorizeWithAccountId updatePgNodeHandler
  :<|> authorizeWithAccountId deletePgNodeHandler


-- * pg file api


type PgFileApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" (Id PgCollection) :> (
    "pgFile" :> (
      -- createPgFile
      ReqBody '[JSON] NewPgFile :> Post '[JSON] ()
    ) :<|> "rootPgFile" :> (
      -- create root pg file
      ReqBody '[JSON] NewRootPgFile :> Post '[JSON] ()
    ) :<|> Capture "pgNodeId" (Id Postgres) :> ReqBody '[JSON] UpdatePgFile :> Put '[JSON] ()
  ))

pgFileApiServer
  :: (AuthResult CookieSession -> Id PgCollection -> NewPgFile -> AppM ())
  :<|> (AuthResult CookieSession -> Id PgCollection -> NewRootPgFile -> AppM ())
  :<|> (AuthResult CookieSession -> Id PgCollection -> Id Postgres -> UpdatePgFile -> AppM ())
pgFileApiServer =
  authorizeWithAccountId createPgFileHandler
  :<|> authorizeWithAccountId createRootPgFileHandler
  :<|> authorizeWithAccountId updatePgFileHandler


-- * pg folder api


type PgFolderApi auths =
  Flat (Auth auths CookieSession :> "api" :> "pgCollection" :> Capture "pgCollectionId" (Id PgCollection) :> (
    "pgFolder" :> (
      -- create pg folder
      ReqBody '[JSON] NewPgFolder :> Post '[JSON] ()
    ) :<|> "rootPgFolder" :> (
      -- create root pg folder
      ReqBody '[JSON] NewRootPgFolder :> Post '[JSON] ()
    )
  ))

pgFolderApiServer
  :: (AuthResult CookieSession -> Id PgCollection -> NewPgFolder -> AppM ())
  :<|> (AuthResult CookieSession -> Id PgCollection -> NewRootPgFolder -> AppM ())
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
      "signIn" :> ReqBody '[JSON] SignInTest :> Post '[JSON] (Headers '[ Header "Set-Cookie" String ] String) :<|>
      "checkSuperSecret" :> Header "Cookie" String :> Get '[JSON] String :<|>
      "deleteNoContent" :> DeleteNoContent '[JSON] NoContent :<|>
      "getStatusCode" :> Capture "statusCode" Int :> Get '[JSON] () :<|>
      "users" :> (
         -- create
        ReqBody '[JSON] NewUserTest :> Post '[JSON] UserTest :<|>
        -- delete
        Capture "userId" Int :> Delete '[JSON] () :<|>
        -- show
        Capture "userId" Int :> Get '[JSON] UserTest :<|>
        -- update
        Capture "userId" Int :> ReqBody '[JSON] UpdateUserTest :> Put '[JSON] UserTest :<|>
        -- update role
        Header "admin" Bool :> Capture "userId" Int :> "role" :> ReqBody '[JSON] UserRole :> Put '[JSON] UserRole :<|>
        -- list
        Get '[JSON] [UserTest]
      ) :<|>
      "products" :> (
         -- create
        ReqBody '[JSON] NewProductTest :> Post '[JSON] ProductTest :<|>
        -- delete
        Capture "productId" Int :> Delete '[JSON] () :<|>
        -- show
        Capture "productId" Int :> Get '[JSON] ProductTest :<|>
        -- update
        Capture "productId" Int :> ReqBody '[JSON] UpdateProductTest :> Put '[JSON] ProductTest :<|>
        -- list
        Get '[JSON] [ProductTest]
      ) :<|>
      "basket" :> (
        -- add to basket
        Capture "userId" Int :> ReqBody '[JSON] AddToBasketTest :> Post '[JSON] () :<|>
        -- remove from basket
        Capture "userId" Int :> ReqBody '[JSON] RemoveFromBasketTest :> Delete '[JSON] () :<|>
        -- show
        Capture "userId" Int :> Get '[JSON] BasketTest
      )
    )
  )


testApiServer :: ServerT TestApi AppM
testApiServer =
  signInHandler :<|>
  checkSuperSecretHandler :<|>
  deleteNoContentHandler :<|>
  getStatusCodeHandler :<|>
  createUserHandler :<|>
  deleteUserHandler :<|>
  showUserHandler :<|>
  updateUserHandler :<|>
  updateRoleHandler :<|>
  listUserHandler :<|>
  createProductHandler :<|>
  deleteProductHandler :<|>
  showProductHandler :<|>
  updateProductHandler :<|>
  listProductHandler :<|>
  addToBasketHandler :<|>
  removeFromBasketHandler :<|>
  showBasketHandler

type AssetApi =
  "public" :> Raw


-- * util


authorizeWithAccountId
  :: Servant.Auth.Server.Internal.ThrowAll.ThrowAll p
  => (Id Account -> p) -> AuthResult CookieSession -> p
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
