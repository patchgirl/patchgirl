{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import qualified Data.Aeson                             as Aeson
import qualified Data.Text                              as T
import           Data.Word
import           Elm.Module                             as Elm
import           Elm.TyRep
import           GHC.TypeLits                           (ErrorMessage (Text),
                                                         KnownSymbol, Symbol,
                                                         TypeError, symbolVal)
import           Servant                                ((:<|>))
import           Servant.API                            ((:>), Capture, Get,
                                                         JSON)
import qualified Servant.API.ContentTypes               as Servant
import           Servant.API.Flatten                    (Flat)
import           Servant.Auth                           (Auth (..), Cookie)
import qualified Servant.Auth.Client                    as Servant
import           Servant.Auth.Server                    (JWT)
import           Servant.Elm
import           Servant.Foreign                        hiding (Static)


import           PatchGirl.Web.Api                      as Web
import           PatchGirl.Web.CaseInsensitive          as Web
import           PatchGirl.Web.ElmOption                (deriveWithSingleFieldObject,
                                                         deriveWithTaggedObject)
import           PatchGirl.Web.Environment.Model        as Web
import           PatchGirl.Web.Github.App               as Web
import           PatchGirl.Web.Health.App               as Web
import           PatchGirl.Web.Http                     as Web
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


instance IsElmDefinition Servant.Token where
  compileElmDef _ = ETypePrimAlias (EPrimAlias (ETypeName "Token" []) (ETyCon (ETCon "String")))

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")

instance
  ( TokenHeaderName auths ~ header
  , KnownSymbol header
  , HasForeignType lang ftype Servant.Token
  , HasForeign lang ftype sub
  , Show ftype
  )
  => HasForeign lang ftype (Auth auths a :> sub) where
    type Foreign ftype (Auth auths a :> sub) = Foreign ftype sub

    foreignFor lang Proxy Proxy req =
      foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
      where
        arg   = Arg
          { _argName = PathSegment . T.pack $ symbolVal @header Proxy
          , _argType = token
          }
        token = typeFor lang (Proxy @ftype) (Proxy @Servant.Token)
        subP  = Proxy @sub

{-
  this is used to a convert parameter in a url to a string
  eg : whatever.com/books/:someUuidToConvertToString?arg=:someOtherComplexTypeToConvertToString
-}
myDefaultElmToString :: EType -> T.Text
myDefaultElmToString argType =
  case argType of
    ETyCon (ETCon "UUID") -> "Uuid.toString"
    _                     -> defaultElmToString argType


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
  , "import Uuid"
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


deriveElmDef deriveWithTaggedObject ''Servant.NoContent


-- ** runner


deriveElmDef deriveWithTaggedObject ''Runner.RequestComputationInput
deriveElmDef deriveWithTaggedObject ''Runner.RequestComputation
deriveElmDef deriveWithTaggedObject ''Runner.RequestComputationOutput
deriveElmDef deriveWithTaggedObject ''Runner.ScriptException
deriveElmDef deriveWithSingleFieldObject ''Runner.HttpException
deriveElmDef deriveWithSingleFieldObject ''Runner.PgComputation
deriveElmDef deriveWithSingleFieldObject ''Runner.SceneComputation
deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioInput
deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioOutput
deriveElmDef deriveWithSingleFieldObject ''Runner.SceneOutput
deriveElmDef deriveWithSingleFieldObject ''Runner.ScenarioVars
deriveElmDef deriveWithTaggedObject ''Runner.Template
deriveElmDef deriveWithSingleFieldObject ''Runner.TemplatedRequestComputationInput
deriveElmDef deriveWithTaggedObject ''Runner.TangoAst
deriveElmDef deriveWithTaggedObject ''Runner.Proc
deriveElmDef deriveWithTaggedObject ''Runner.Expr
deriveElmDef deriveWithTaggedObject ''Runner.Json
deriveElmDef deriveWithTaggedObject ''Runner.EnvironmentVars
deriveElmDef deriveWithTaggedObject ''Runner.StringTemplate
deriveElmDef deriveWithTaggedObject ''Runner.Row
deriveElmDef deriveWithTaggedObject ''Runner.Column
deriveElmDef deriveWithSingleFieldObject ''Runner.PgValue
deriveElmDef deriveWithTaggedObject ''Runner.PgComputationInput
deriveElmDef deriveWithTaggedObject ''Runner.TemplatedPgConnection
deriveElmDef deriveWithTaggedObject ''Runner.SceneFile
deriveElmDef deriveWithTaggedObject ''Runner.PgError
deriveElmDef deriveWithTaggedObject ''Runner.PgComputationOutput


-- ** web


deriveElmDef deriveWithTaggedObject ''Web.RequestCollection
deriveElmDef deriveWithTaggedObject ''Web.RequestNode
deriveElmDef deriveWithTaggedObject ''Web.Method
deriveElmDef deriveWithTaggedObject ''Web.AppHealth
deriveElmDef deriveWithTaggedObject ''Web.NewRequestFile
deriveElmDef deriveWithTaggedObject ''Web.ParentNodeId
deriveElmDef deriveWithTaggedObject ''Web.UpdateRequestNode
deriveElmDef deriveWithTaggedObject ''Web.NewEnvironment
deriveElmDef deriveWithTaggedObject ''Web.UpdateEnvironment
deriveElmDef deriveWithTaggedObject ''Web.Environment
deriveElmDef deriveWithTaggedObject ''Web.KeyValue
deriveElmDef deriveWithTaggedObject ''Web.NewKeyValue
deriveElmDef deriveWithTaggedObject ''Web.CaseInsensitive
deriveElmDef deriveWithTaggedObject ''Web.Session
deriveElmDef deriveWithTaggedObject ''Web.Scheme
deriveElmDef deriveWithTaggedObject ''Web.NewRequestFolder
deriveElmDef deriveWithTaggedObject ''Web.NewRootRequestFile
deriveElmDef deriveWithTaggedObject ''Web.NewRootRequestFolder
deriveElmDef deriveWithTaggedObject ''Web.UpdateRequestFile
deriveElmDef deriveWithTaggedObject ''Web.HttpHeader
deriveElmDef deriveWithTaggedObject ''Web.SignInWithGithub
deriveElmDef deriveWithTaggedObject ''Web.NewScenarioFile
deriveElmDef deriveWithTaggedObject ''Web.UpdateScenarioNode
deriveElmDef deriveWithTaggedObject ''Web.NewScenarioFolder
deriveElmDef deriveWithTaggedObject ''Web.NewRootScenarioFile
deriveElmDef deriveWithTaggedObject ''Web.NewRootScenarioFolder
deriveElmDef deriveWithTaggedObject ''Web.ScenarioCollection
deriveElmDef deriveWithTaggedObject ''Web.ScenarioNode
deriveElmDef deriveWithTaggedObject ''Web.SceneActor
deriveElmDef deriveWithTaggedObject ''Web.NewScene
deriveElmDef deriveWithTaggedObject ''Web.UpdateScenarioFile
deriveElmDef deriveWithTaggedObject ''Web.UpdateScene
deriveElmDef deriveWithTaggedObject ''Web.PgCollection
deriveElmDef deriveWithTaggedObject ''Web.PgNode
deriveElmDef deriveWithTaggedObject ''Web.UpdatePgNode
deriveElmDef deriveWithTaggedObject ''Web.NewPgFile
deriveElmDef deriveWithTaggedObject ''Web.UpdatePgFile
deriveElmDef deriveWithTaggedObject ''Web.NewRootPgFile
deriveElmDef deriveWithTaggedObject ''Web.NewRootPgFolder
deriveElmDef deriveWithTaggedObject ''Web.NewPgFolder
deriveElmDef deriveWithTaggedObject ''Web.ActorType
deriveElmDef deriveWithTaggedObject ''Web.Variables


-- * main


-- ** web


webModule :: IO ()
webModule =
  let
    options :: ElmOptions
    options =
      defElmOptions { urlPrefix = Dynamic
                    , stringElmTypes =
                      [ toElmType (Proxy @String)
                      , toElmType (Proxy @T.Text)
                      , toElmType (Proxy @Servant.Token)
                      ]
                    , elmToString = myDefaultElmToString
                    }
    namespace =
      [ "Api"
      , "WebGeneratedClient"
      ]
    targetFolder = "../front/elm"
    elmDefinitions =
      [ DefineElm (Proxy :: Proxy Servant.NoContent)
      , DefineElm (Proxy :: Proxy Servant.Token)
      , DefineElm (Proxy :: Proxy Web.RequestCollection)
      , DefineElm (Proxy :: Proxy Web.RequestNode)
      , DefineElm (Proxy :: Proxy Web.Method)
      , DefineElm (Proxy :: Proxy Web.AppHealth)
      , DefineElm (Proxy :: Proxy Web.NewRequestFile)
      , DefineElm (Proxy :: Proxy Web.ParentNodeId)
      , DefineElm (Proxy :: Proxy Web.UpdateRequestNode)
      , DefineElm (Proxy :: Proxy Web.NewEnvironment)
      , DefineElm (Proxy :: Proxy Web.UpdateEnvironment)
      , DefineElm (Proxy :: Proxy Web.Environment)
      , DefineElm (Proxy :: Proxy Web.KeyValue)
      , DefineElm (Proxy :: Proxy Web.NewKeyValue)
      , DefineElm (Proxy :: Proxy Web.CaseInsensitive)
      , DefineElm (Proxy :: Proxy Web.Session)
      , DefineElm (Proxy :: Proxy Web.Scheme)
      , DefineElm (Proxy :: Proxy Web.NewRequestFolder)
      , DefineElm (Proxy :: Proxy Web.NewRootRequestFile)
      , DefineElm (Proxy :: Proxy Web.NewRootRequestFolder)
      , DefineElm (Proxy :: Proxy Web.UpdateRequestFile)
      , DefineElm (Proxy :: Proxy Web.HttpHeader)
      , DefineElm (Proxy :: Proxy Web.SignInWithGithub)
      , DefineElm (Proxy :: Proxy Web.NewScenarioFile)
      , DefineElm (Proxy :: Proxy Web.UpdateScenarioNode)
      , DefineElm (Proxy :: Proxy Web.NewScenarioFolder)
      , DefineElm (Proxy :: Proxy Web.NewRootScenarioFolder)
      , DefineElm (Proxy :: Proxy Web.NewRootScenarioFile)
      , DefineElm (Proxy :: Proxy Web.ScenarioCollection)
      , DefineElm (Proxy :: Proxy Web.ScenarioNode)
      , DefineElm (Proxy :: Proxy Web.SceneActor)
      , DefineElm (Proxy :: Proxy Web.NewScene)
      , DefineElm (Proxy :: Proxy Web.UpdateScenarioFile)
      , DefineElm (Proxy :: Proxy Web.UpdateScene)
      , DefineElm (Proxy :: Proxy Web.PgCollection)
      , DefineElm (Proxy :: Proxy Web.PgNode)
      , DefineElm (Proxy :: Proxy Web.UpdatePgNode)
      , DefineElm (Proxy :: Proxy Web.NewPgFile)
      , DefineElm (Proxy :: Proxy Web.UpdatePgFile)
      , DefineElm (Proxy :: Proxy Web.NewRootPgFile)
      , DefineElm (Proxy :: Proxy Web.NewRootPgFolder)
      , DefineElm (Proxy :: Proxy Web.NewPgFolder)
      , DefineElm (Proxy :: Proxy Web.ActorType)
      , DefineElm (Proxy :: Proxy Web.Variables)
      ]
    proxyApi =
      (Proxy :: Proxy (Web.RestApi '[Cookie]))
  in
    generateElmModuleWith options namespace webCustomCode targetFolder elmDefinitions proxyApi


-- ** runner


runnerElmModule :: IO ()
runnerElmModule =
  let
    options :: ElmOptions
    options =
      defElmOptions { urlPrefix = Dynamic
                    , stringElmTypes =
                      [ toElmType (Proxy @String)
                      , toElmType (Proxy @T.Text)
                      , toElmType (Proxy @Servant.Token)
                      ]
                    , elmToString = myDefaultElmToString
                    }
    namespace =
      [ "Api"
      , "RunnerGeneratedClient"
      ]
    targetFolder = "../front/elm"
    elmDefinitions =
      [ DefineElm (Proxy :: Proxy Runner.RequestComputationInput)
      , DefineElm (Proxy :: Proxy Runner.RequestComputation)
      , DefineElm (Proxy :: Proxy Runner.RequestComputationOutput)
      , DefineElm (Proxy :: Proxy Runner.ScriptException)
      , DefineElm (Proxy :: Proxy Runner.HttpException)
      , DefineElm (Proxy :: Proxy Runner.Template)
      , DefineElm (Proxy :: Proxy Runner.StringTemplate)
      , DefineElm (Proxy :: Proxy Runner.TemplatedRequestComputationInput)
      , DefineElm (Proxy :: Proxy Runner.ScenarioOutput)
      , DefineElm (Proxy :: Proxy Runner.ScenarioInput)
      , DefineElm (Proxy :: Proxy Runner.SceneOutput)
      , DefineElm (Proxy :: Proxy Runner.Expr)
      , DefineElm (Proxy :: Proxy Runner.TangoAst)
      , DefineElm (Proxy :: Proxy Runner.Json)
      , DefineElm (Proxy :: Proxy Runner.Row)
      , DefineElm (Proxy :: Proxy Runner.Proc)
      , DefineElm (Proxy :: Proxy Runner.EnvironmentVars)
      , DefineElm (Proxy :: Proxy Runner.SceneComputation)
      , DefineElm (Proxy :: Proxy Runner.PgComputation)
      , DefineElm (Proxy :: Proxy Runner.Column)
      , DefineElm (Proxy :: Proxy Runner.PgValue)
      , DefineElm (Proxy :: Proxy Runner.PgComputationInput)
      , DefineElm (Proxy :: Proxy Runner.TemplatedPgConnection)
      , DefineElm (Proxy :: Proxy Runner.SceneFile)
      , DefineElm (Proxy :: Proxy Runner.PgError)
      , DefineElm (Proxy :: Proxy Runner.PgComputationOutput)
      ]
    proxyApi =
      (Proxy :: Proxy Runner.RunnerApi)
  in
    generateElmModuleWith options namespace runnerCustomCode targetFolder elmDefinitions proxyApi


-- ** main


main :: IO ()
main = do
  runnerElmModule
  webModule
