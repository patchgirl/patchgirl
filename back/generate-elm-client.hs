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


-- * third party lib


import           Control.Lens              ((&), (<>~))
import qualified Data.Aeson                as Aeson
import qualified Data.Text                 as T
import           Data.Word
import           Elm.Module                as Elm
import           Elm.TyRep
import           ElmOption                 (deriveWithSingleFieldObject,
                                            deriveWithTaggedObject)
import           GHC.TypeLits              (ErrorMessage (Text), KnownSymbol,
                                            Symbol, TypeError, symbolVal)


-- * patchgirl lib


import           App
import           Environment.Model
import           Github.App
import           Health.App
import           Http
import           Interpolator
import           Model                     (CaseInsensitive)
import           RequestCollection.Model
import           RequestComputation.Model
import           RequestNode.Model
import           ScenarioCollection.Model
import           ScenarioComputation.Model
import           ScenarioNode.Model
import           Servant                   ((:<|>))
import           Servant.API               ((:>), Capture, Get, JSON)
import           Servant.API.ContentTypes  (NoContent)
import           Servant.API.Flatten       (Flat)
import           Servant.Auth              (Auth (..), Cookie)
import           Servant.Auth.Client       (Token)
import           Servant.Auth.Server       (JWT)
import           Servant.Elm
import           Servant.Foreign           hiding (Static)
import           Session.Model
import           TangoScript

-- * util


instance IsElmDefinition Token where
  compileElmDef _ = ETypePrimAlias (EPrimAlias (ETypeName "Token" []) (ETyCon (ETCon "String")))

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")

instance
  ( TokenHeaderName auths ~ header
  , KnownSymbol header
  , HasForeignType lang ftype Token
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
        token = typeFor lang (Proxy @ftype) (Proxy @Token)
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


customCode :: T.Text
customCode = T.unlines
  [ "import Json.Decode"
  , "import Json.Encode exposing (Value)"
  , "-- The following module comes from bartavelle/json-helpers"
  , "import Json.Helpers exposing (..)"
  , "import Dict exposing (Dict)"
  , "import Set"
  , "import Http"
  , "import String"
  , "import Url.Builder"
  , "import Uuid as Uuid"
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
  , "  Json.Decode.oneOf [ decoder1 |> Json.Decode.map Err"
  , "                    , decoder2 |> Json.Decode.map Ok"
  , "                    ]"
  , ""
  , "jsonEncEither : (a -> Value) -> (b -> Value) -> Either a b -> Value"
  , "jsonEncEither encoder1 encoder2 either ="
  , "  case either of"
  , "    Err err -> encoder1 err"
  , "    Ok ok -> encoder2 ok"
  , ""
  ]


-- * elm def


deriveElmDef deriveWithTaggedObject ''RequestCollection
deriveElmDef deriveWithTaggedObject ''RequestNode
deriveElmDef deriveWithTaggedObject ''Method
deriveElmDef deriveWithTaggedObject ''AppHealth
deriveElmDef deriveWithTaggedObject ''NoContent
deriveElmDef deriveWithTaggedObject ''NewRequestFile
deriveElmDef deriveWithTaggedObject ''ParentNodeId
deriveElmDef deriveWithTaggedObject ''UpdateRequestNode
deriveElmDef deriveWithTaggedObject ''NewEnvironment
deriveElmDef deriveWithTaggedObject ''UpdateEnvironment
deriveElmDef deriveWithTaggedObject ''Environment
deriveElmDef deriveWithTaggedObject ''KeyValue
deriveElmDef deriveWithTaggedObject ''NewKeyValue
deriveElmDef deriveWithTaggedObject ''CaseInsensitive
deriveElmDef deriveWithTaggedObject ''Session
deriveElmDef deriveWithTaggedObject ''RequestComputationInput
deriveElmDef deriveWithTaggedObject ''RequestComputationOutput
deriveElmDef deriveWithTaggedObject ''RequestComputationResult
deriveElmDef deriveWithTaggedObject ''Scheme
deriveElmDef deriveWithTaggedObject ''NewRequestFolder
deriveElmDef deriveWithTaggedObject ''NewRootRequestFile
deriveElmDef deriveWithTaggedObject ''NewRootRequestFolder
deriveElmDef deriveWithTaggedObject ''UpdateRequestFile
deriveElmDef deriveWithTaggedObject ''HttpHeader
deriveElmDef deriveWithTaggedObject ''SignInWithGithub
deriveElmDef deriveWithTaggedObject ''NewScenarioFile
deriveElmDef deriveWithTaggedObject ''UpdateScenarioNode
deriveElmDef deriveWithTaggedObject ''NewScenarioFolder
deriveElmDef deriveWithTaggedObject ''NewRootScenarioFile
deriveElmDef deriveWithTaggedObject ''NewRootScenarioFolder
deriveElmDef deriveWithTaggedObject ''ScenarioCollection
deriveElmDef deriveWithTaggedObject ''ScenarioNode
deriveElmDef deriveWithTaggedObject ''Scene
deriveElmDef deriveWithTaggedObject ''NewScene
deriveElmDef deriveWithTaggedObject ''ScriptException
deriveElmDef deriveWithSingleFieldObject ''HttpException
deriveElmDef deriveWithSingleFieldObject ''SceneComputation
deriveElmDef deriveWithSingleFieldObject ''Proc
deriveElmDef deriveWithSingleFieldObject ''Expr
deriveElmDef deriveWithSingleFieldObject ''ScenarioInput
deriveElmDef deriveWithSingleFieldObject ''ScenarioOutput
deriveElmDef deriveWithSingleFieldObject ''SceneOutput
deriveElmDef deriveWithSingleFieldObject ''SceneInput
deriveElmDef deriveWithSingleFieldObject ''TangoAst
deriveElmDef deriveWithSingleFieldObject ''ScenarioEnvironment
deriveElmDef deriveWithTaggedObject ''Template
deriveElmDef deriveWithSingleFieldObject ''TemplatedRequestComputationInput


-- * main


main :: IO ()
main =
  let
    options :: ElmOptions
    options =
      defElmOptions { urlPrefix = Dynamic
                    , stringElmTypes =
                      [ toElmType (Proxy @String)
                      , toElmType (Proxy @T.Text)
                      , toElmType (Proxy @Token)
                      ]
                    , elmToString = myDefaultElmToString
                    }
    namespace =
      [ "Api"
      , "Generated"
      ]
    targetFolder = "../front/elm"
    elmDefinitions =
      [ DefineElm (Proxy :: Proxy RequestCollection)
      , DefineElm (Proxy :: Proxy RequestNode)
      , DefineElm (Proxy :: Proxy Method)
      , DefineElm (Proxy :: Proxy AppHealth)
      , DefineElm (Proxy :: Proxy NoContent)
      , DefineElm (Proxy :: Proxy NewRequestFile)
      , DefineElm (Proxy :: Proxy ParentNodeId)
      , DefineElm (Proxy :: Proxy UpdateRequestNode)
      , DefineElm (Proxy :: Proxy NewEnvironment)
      , DefineElm (Proxy :: Proxy UpdateEnvironment)
      , DefineElm (Proxy :: Proxy Environment)
      , DefineElm (Proxy :: Proxy KeyValue)
      , DefineElm (Proxy :: Proxy NewKeyValue)
      , DefineElm (Proxy :: Proxy CaseInsensitive)
      , DefineElm (Proxy :: Proxy Session)
      , DefineElm (Proxy :: Proxy Token)
      , DefineElm (Proxy :: Proxy RequestComputationInput)
      , DefineElm (Proxy :: Proxy RequestComputationOutput)
      , DefineElm (Proxy :: Proxy RequestComputationResult)
      , DefineElm (Proxy :: Proxy Scheme)
      , DefineElm (Proxy :: Proxy NewRequestFolder)
      , DefineElm (Proxy :: Proxy NewRootRequestFile)
      , DefineElm (Proxy :: Proxy NewRootRequestFolder)
      , DefineElm (Proxy :: Proxy UpdateRequestFile)
      , DefineElm (Proxy :: Proxy HttpHeader)
      , DefineElm (Proxy :: Proxy SignInWithGithub)
      , DefineElm (Proxy :: Proxy NewScenarioFile)
      , DefineElm (Proxy :: Proxy UpdateScenarioNode)
      , DefineElm (Proxy :: Proxy NewScenarioFolder)
      , DefineElm (Proxy :: Proxy NewRootScenarioFolder)
      , DefineElm (Proxy :: Proxy NewRootScenarioFile)
      , DefineElm (Proxy :: Proxy ScenarioCollection)
      , DefineElm (Proxy :: Proxy ScenarioNode)
      , DefineElm (Proxy :: Proxy Scene)
      , DefineElm (Proxy :: Proxy NewScene)
      , DefineElm (Proxy :: Proxy HttpException)
      , DefineElm (Proxy :: Proxy SceneComputation)
      , DefineElm (Proxy :: Proxy ScriptException)
      , DefineElm (Proxy :: Proxy Proc)
      , DefineElm (Proxy :: Proxy Expr)
      , DefineElm (Proxy :: Proxy ScenarioInput)
      , DefineElm (Proxy :: Proxy ScenarioOutput)
      , DefineElm (Proxy :: Proxy SceneOutput)
      , DefineElm (Proxy :: Proxy SceneInput)
      , DefineElm (Proxy :: Proxy TangoAst)
      , DefineElm (Proxy :: Proxy ScenarioEnvironment)
      , DefineElm (Proxy :: Proxy Template)
      , DefineElm (Proxy :: Proxy TemplatedRequestComputationInput)
      ]
    proxyApi =
      (Proxy :: Proxy (RestApi '[Cookie]))
  in
    generateElmModuleWith options namespace customCode targetFolder elmDefinitions proxyApi
