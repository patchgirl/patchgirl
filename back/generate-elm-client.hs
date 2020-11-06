{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}


import           Control.Lens                           ((&), (<>~))
import qualified Data.Text                              as T
import           Debug.Trace
import qualified Elm.Derive                             as Elm
import           Elm.Module                             as Elm
import           Elm.TyRep                              (ETCon, EType, ETypeDef)
import qualified Elm.TyRep                              as Elm
import           GHC.TypeLits                           (ErrorMessage (Text),
                                                         KnownSymbol, Symbol,
                                                         TypeError, symbolVal)
import qualified Servant.API.ContentTypes               as Servant
import qualified Servant.Auth                           as Servant
import qualified Servant.Auth.Client                    as Servant
import qualified Servant.Auth.Server                    as Servant
import qualified Servant.Elm                            as Elm
import           Servant.Foreign                        hiding (Static)

import           PatchGirl.Web.Api                      as Web
import           PatchGirl.Web.CaseInsensitive          as Web
import           PatchGirl.Web.ElmOption                (deriveWithSingleFieldObject,
                                                         deriveWithTaggedObject)
import           PatchGirl.Web.Environment.Model        as Web
import           PatchGirl.Web.Github.App               as Web
import           PatchGirl.Web.Health.App               as Web
import           PatchGirl.Web.Http                     as Web
import           PatchGirl.Web.Id                       as Web
import           PatchGirl.Web.Internal.Env             as Web
import           PatchGirl.Web.PgCollection.Model       as Web
import           PatchGirl.Web.PgNode.Model             as Web
import           PatchGirl.Web.RequestCollection.Model  as Web
import           PatchGirl.Web.RequestNode.Model        as Web
import           PatchGirl.Web.ScenarioCollection.Model as Web
import           PatchGirl.Web.ScenarioNode.Model       as Web
import           PatchGirl.Web.Session.Model            as Web

import qualified Api                                    as Runner
import qualified Interpolator                           as Runner
import qualified PgSqlComputation.Model                 as Runner
import qualified RequestComputation.Model               as Runner
import qualified ScenarioComputation.Model              as Runner
import qualified TangoScript.Model                      as Runner


-- * util


instance Elm.IsElmDefinition Servant.Token where
  compileElmDef _ = Elm.ETypePrimAlias (Elm.EPrimAlias (Elm.ETypeName "Token" []) (Elm.ETyCon (Elm.ETCon "String")))

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Servant.Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (Servant.JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")

instance
  ( TokenHeaderName auths ~ header
  , KnownSymbol header
  , HasForeignType lang ftype Servant.Token
  , HasForeign lang ftype sub
  , Show ftype
  )
  => HasForeign lang ftype (Servant.Auth auths a :> sub) where
    type Foreign ftype (Servant.Auth auths a :> sub) = Foreign ftype sub

    foreignFor lang Elm.Proxy Elm.Proxy req =
      foreignFor lang Elm.Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
      where
        arg   = Arg
          { _argName = PathSegment . T.pack $ symbolVal @header Elm.Proxy
          , _argType = token
          }
        token = typeFor lang (Elm.Proxy @ftype) (Elm.Proxy @Servant.Token)
        subP  = Elm.Proxy @sub

{-
  this is used to a convert parameter in a url to a string
  eg : whatever.com/books/:someUuidToConvertToString?arg=:someOtherComplexTypeToConvertToString
-}
myDefaultElmToString :: Elm.EType -> T.Text
myDefaultElmToString argType =
  case argType of
    Elm.ETyCon (Elm.ETCon "UUID")    -> "Uuid.toString"
    Elm.ETyApp (Elm.ETyCon Elm.ETCon { Elm.tc_name = "Id" }) (Elm.ETyCon (Elm.ETCon _)) -> "\\(Id a) -> Uuid.toString a"
    _ ->  Elm.defaultElmToString argType


-- * custom code


-- ** web


webCustomCode :: T.Text
webCustomCode = T.unlines
  [ "import Json.Decode"
  , "import Json.Encode exposing (Value)"
  , "-- The following module comes from bartavelle/json-helpers"
  , "import Json.Helpers exposing (..)"
  , "import Dict exposing (Dict)"
  , "import Set"
  , "import Http"
  , "import String"
  , "import Url.Builder"
  , "import Uuid exposing (Uuid)"
  , ""
  , "jsonDecUuid : Json.Decode.Decoder Uuid.Uuid"
  , "jsonDecUuid = Uuid.decoder"
  , ""
  , "jsonEncUuid : Uuid.Uuid -> Value"
  , "jsonEncUuid = Uuid.encode"
  , ""
  , "type alias Either a b = Result a b"
  , ""
  , "jsonDecEither : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Either a b)"
  , "jsonDecEither decoder1 decoder2 ="
  , "  Json.Decode.oneOf [ Json.Decode.field \"Left\" decoder1 |> Json.Decode.map Err"
  , "                    , Json.Decode.field \"Right\" decoder2 |> Json.Decode.map Ok"
  , "                    ]"
  , ""
  , "jsonEncEither : (a -> Value) -> (b -> Value) -> Either a b -> Value"
  , "jsonEncEither encoder1 encoder2 either ="
  , "  case either of"
  , "    Err err -> encoder1 err"
  , "    Ok ok -> encoder2 ok"
  , ""
  ]


-- ** runner


runnerCustomCode :: T.Text
runnerCustomCode = T.unlines
  [ "import Json.Decode"
  , "import Json.Encode exposing (Value)"
  , "-- The following module comes from bartavelle/json-helpers"
  , "import Json.Helpers exposing (..)"
  , "import Dict exposing (Dict)"
  , "import Set"
  , "import Http"
  , "import String"
  , "import Url.Builder"
  , "import Uuid"
  , "import Api.WebGeneratedClient exposing(..)"
  , ""
  , "type alias UUID = Uuid.Uuid"
  , ""
  , "jsonDecUUID : Json.Decode.Decoder UUID"
  , "jsonDecUUID = Uuid.decoder"
  , ""
  , "jsonEncUUID : UUID -> Value"
  , "jsonEncUUID = Uuid.encode"
  , ""
  , "type alias Either a b = Result a b"
  , ""
  , "jsonDecEither : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Either a b)"
  , "jsonDecEither decoder1 decoder2 ="
  , "  Json.Decode.oneOf [ Json.Decode.field \"Left\" decoder1 |> Json.Decode.map Err"
  , "                    , Json.Decode.field \"Right\" decoder2 |> Json.Decode.map Ok"
  , "                    ]"
  , ""
  , "jsonEncEither : (a -> Value) -> (b -> Value) -> Either a b -> Value"
  , "jsonEncEither encoder1 encoder2 either ="
  , "  case either of"
  , "    Err err -> encoder1 err"
  , "    Ok ok -> encoder2 ok"
  ]


-- * elm def


Elm.deriveElmDef deriveWithTaggedObject ''Servant.NoContent


-- ** runner


Elm.deriveElmDef deriveWithTaggedObject ''Runner.RequestComputationInput
Elm.deriveElmDef deriveWithTaggedObject ''Runner.RequestComputation
Elm.deriveElmDef deriveWithTaggedObject ''Runner.RequestComputationOutput
Elm.deriveElmDef deriveWithTaggedObject ''Runner.ScriptException
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.HttpException
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.PgComputation
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.SceneComputation
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioInput
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioOutput
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.SceneOutput
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioVars
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Template
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.TemplatedRequestComputationInput
Elm.deriveElmDef deriveWithTaggedObject ''Runner.TangoAst
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Proc
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Expr
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Json
Elm.deriveElmDef deriveWithTaggedObject ''Runner.EnvironmentVars
Elm.deriveElmDef deriveWithTaggedObject ''Runner.StringTemplate
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Row
Elm.deriveElmDef deriveWithTaggedObject ''Runner.Column
Elm.deriveElmDef deriveWithSingleFieldObject ''Runner.PgValue
Elm.deriveElmDef deriveWithTaggedObject ''Runner.PgComputationInput
Elm.deriveElmDef deriveWithTaggedObject ''Runner.TemplatedPgConnection
Elm.deriveElmDef deriveWithTaggedObject ''Runner.SceneFile
Elm.deriveElmDef deriveWithTaggedObject ''Runner.PgError
Elm.deriveElmDef deriveWithTaggedObject ''Runner.PgComputationOutput


-- ** web


Elm.deriveElmDef deriveWithTaggedObject ''Web.RequestCollection
Elm.deriveElmDef deriveWithTaggedObject ''Web.RequestNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.Method
Elm.deriveElmDef deriveWithTaggedObject ''Web.AppHealth
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRequestFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.ParentNodeId
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateRequestNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewEnvironment
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateEnvironment
Elm.deriveElmDef deriveWithTaggedObject ''Web.Environment
Elm.deriveElmDef deriveWithTaggedObject ''Web.KeyValue
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewKeyValue
Elm.deriveElmDef deriveWithTaggedObject ''Web.CaseInsensitive
Elm.deriveElmDef deriveWithTaggedObject ''Web.Session
Elm.deriveElmDef deriveWithTaggedObject ''Web.Scheme
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRequestFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootRequestFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootRequestFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateRequestFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.HttpHeader
Elm.deriveElmDef deriveWithTaggedObject ''Web.SignInWithGithub
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewScenarioFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateScenarioNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewScenarioFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootScenarioFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootScenarioFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.ScenarioCollection
Elm.deriveElmDef deriveWithTaggedObject ''Web.ScenarioNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.SceneActor
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewScene
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateScenarioFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdateScene
Elm.deriveElmDef deriveWithTaggedObject ''Web.PgCollection
Elm.deriveElmDef deriveWithTaggedObject ''Web.PgNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdatePgNode
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewPgFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.UpdatePgFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootPgFile
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewRootPgFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.NewPgFolder
Elm.deriveElmDef deriveWithTaggedObject ''Web.ActorType
Elm.deriveElmDef deriveWithTaggedObject ''Web.SceneVariables
Elm.deriveElmDef deriveWithTaggedObject ''Web.SceneVariableValue
Elm.deriveElmDef deriveWithTaggedObject ''Web.Request
Elm.deriveElmDef deriveWithTaggedObject ''Web.Account
Elm.deriveElmDef deriveWithTaggedObject ''Web.Postgres
Elm.deriveElmDef deriveWithTaggedObject ''Web.EnvId
Elm.deriveElmDef deriveWithTaggedObject ''Web.KeyValueId
Elm.deriveElmDef deriveWithTaggedObject ''Web.Scenario
Elm.deriveElmDef deriveWithTaggedObject ''Web.Scene
Elm.deriveElmDef deriveWithTaggedObject ''Web.ActorId
Elm.deriveElmDef deriveWithTaggedObject ''Web.ScenarioCol

{-
'Id a' is a phantom type and elm-bridge doesn't handle them
let's create an instance of IsElmDefinition so it can be generated
Note: encoders and decoders will work but it's really hacky,
only the inner UUID is passed as JSON so the phantom type guarantee is thrown out...
-}
instance Elm.IsElmDefinition (Web.Id a) where
  compileElmDef :: Elm.Proxy (Web.Id a) -> ETypeDef
  compileElmDef proxy =
    Elm.ETypeSum $ Elm.ESum { Elm.es_name = Elm.ETypeName "Id" [Elm.ETVar "a"]
                            , Elm.es_constructors = [ Elm.STC { Elm._stcName = "Id"
                                                              , Elm._stcEncoded = "Id"
                                                              , Elm._stcFields = Elm.Anonymous [Elm.ETyCon $ Elm.ETCon "Uuid"]
                                                              }
                                                    ]
                            , Elm.es_type = Elm.SumEncoding' Elm.TwoElemArray
                            , Elm.es_omit_null = True
                            , Elm.es_unary_strings = False
                            }


-- * main


-- ** web


myAlterations :: Elm.ETypeDef -> Elm.ETypeDef
myAlterations = Elm.recAlterType myTypeAlterations

myTypeAlterations :: EType -> EType
myTypeAlterations t =
  case t of
    Elm.ETyCon (Elm.ETCon "UUID") -> Elm.ETyCon (Elm.ETCon "Uuid") -- Backend uuid type is capital cased (UUID) where as frontend uuid is capitalized (Uuid)
    _                             -> Elm.defaultTypeAlterations t

webModule :: IO ()
webModule =
  let
    options :: Elm.ElmOptions
    options =
      Elm.ElmOptions { Elm.urlPrefix = Elm.Dynamic
                     , Elm.elmTypeAlterations = myTypeAlterations
                     , Elm.elmAlterations = myAlterations
                     , Elm.elmToString = myDefaultElmToString
                     , Elm.emptyResponseElmTypes = [ Elm.toElmType (Elm.Proxy :: Elm.Proxy ()) ]
                     , Elm.stringElmTypes = [ Elm.toElmType (Elm.Proxy @String)
                                            , Elm.toElmType (Elm.Proxy @T.Text)
                                            , Elm.toElmType (Elm.Proxy @Servant.Token)
                                            ]
                     }
    namespace =
      [ "Api"
      , "WebGeneratedClient"
      ]
    targetFolder = "../front/elm"
    elmDefinitions =
      [ DefineElm (Elm.Proxy :: Elm.Proxy Servant.NoContent)
      , DefineElm (Elm.Proxy :: Elm.Proxy Servant.Token)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.RequestCollection)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.RequestNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Method)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.AppHealth)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRequestFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ParentNodeId)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateRequestNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewEnvironment)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateEnvironment)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Environment)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.KeyValue)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewKeyValue)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.CaseInsensitive)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Session)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Scheme)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRequestFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootRequestFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootRequestFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateRequestFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.HttpHeader)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.SignInWithGithub)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewScenarioFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateScenarioNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewScenarioFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootScenarioFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootScenarioFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ScenarioCollection)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ScenarioNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.SceneActor)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewScene)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateScenarioFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdateScene)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.PgCollection)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.PgNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdatePgNode)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewPgFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.UpdatePgFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootPgFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewRootPgFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.NewPgFolder)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ActorType)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.SceneVariables)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.SceneVariableValue)
      , DefineElm (Elm.Proxy :: Elm.Proxy (Web.Id a))
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Request)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Account)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Postgres)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.EnvId)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.KeyValueId)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Scenario)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.Scene)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ActorId)
      , DefineElm (Elm.Proxy :: Elm.Proxy Web.ScenarioCol)
      ]
    proxyApi =
      (Elm.Proxy :: Elm.Proxy (Web.RestApi '[Servant.Cookie]))
  in
    Elm.generateElmModuleWith options namespace webCustomCode targetFolder elmDefinitions proxyApi


-- ** runner


runnerElmModule :: IO ()
runnerElmModule =
  let
    options :: Elm.ElmOptions
    options =
      Elm.defElmOptions { Elm.urlPrefix = Elm.Dynamic
                        , Elm.stringElmTypes =
                          [ Elm.toElmType (Elm.Proxy @String)
                          , Elm.toElmType (Elm.Proxy @T.Text)
                          , Elm.toElmType (Elm.Proxy @Servant.Token)
                          ]
                        , Elm.elmToString = myDefaultElmToString
                        }
    namespace =
      [ "Api"
      , "RunnerGeneratedClient"
      ]
    targetFolder = "../front/elm"
    elmDefinitions =
      [ DefineElm (Elm.Proxy :: Elm.Proxy Runner.RequestComputationInput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.RequestComputation)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.RequestComputationOutput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.ScriptException)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.HttpException)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Template)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.StringTemplate)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.TemplatedRequestComputationInput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.ScenarioOutput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.ScenarioInput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.SceneOutput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Expr)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.TangoAst)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Json)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Row)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Proc)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.EnvironmentVars)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.SceneComputation)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.PgComputation)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.Column)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.PgValue)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.PgComputationInput)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.TemplatedPgConnection)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.SceneFile)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.PgError)
      , DefineElm (Elm.Proxy :: Elm.Proxy Runner.PgComputationOutput)
      ]
    proxyApi =
      (Elm.Proxy :: Elm.Proxy Runner.RunnerApi)
  in
    Elm.generateElmModuleWith options namespace runnerCustomCode targetFolder elmDefinitions proxyApi


-- ** main


main :: IO ()
main = do
  runnerElmModule
  webModule
