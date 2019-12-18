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

import           Account.Model
import           App
import           AppHealth
import           Control.Lens             ((&), (<>~))
import qualified Data.Aeson               as Aeson
import qualified Data.Text                as T
import           Elm.Module               as Elm
import           Elm.TyRep

import           Debug.Trace
import           ElmOption                (deriveElmDefOption)
import           Environment.App
import           GHC.TypeLits             (ErrorMessage (Text), KnownSymbol,
                                           Symbol, TypeError, symbolVal)
import           Http
import           Model
import           RequestCollection
import           RequestNode.Model
import           Servant                  ((:<|>))
import           Servant.API              ((:>), Capture, Get, JSON)
import           Servant.API.ContentTypes (NoContent)
import           Servant.API.Flatten      (Flat)
import           Servant.Auth             (Auth (..), Cookie)
import           Servant.Auth.Client      (Token)
import           Servant.Auth.Server      (JWT)
import           Servant.Elm
import           Servant.Foreign          hiding (Static)
import           Session.Model

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

-- input
deriveElmDef deriveElmDefOption ''RequestCollection
deriveElmDef deriveElmDefOption ''RequestNode
deriveElmDef deriveElmDefOption ''Method
deriveElmDef deriveElmDefOption ''AppHealth
deriveElmDef deriveElmDefOption ''NoContent
deriveElmDef deriveElmDefOption ''NewRequestFile
deriveElmDef deriveElmDefOption ''ParentNodeId
deriveElmDef deriveElmDefOption ''UpdateRequestNode
deriveElmDef deriveElmDefOption ''NewEnvironment
deriveElmDef deriveElmDefOption ''UpdateEnvironment
deriveElmDef deriveElmDefOption ''Environment
deriveElmDef deriveElmDefOption ''KeyValue
deriveElmDef deriveElmDefOption ''NewKeyValue
deriveElmDef deriveElmDefOption ''Login
deriveElmDef deriveElmDefOption ''CaseInsensitive
deriveElmDef deriveElmDefOption ''Account
deriveElmDef deriveElmDefOption ''Session
deriveElmDef deriveElmDefOption ''SignUp


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
      , DefineElm (Proxy :: Proxy Login)
      , DefineElm (Proxy :: Proxy CaseInsensitive)
      , DefineElm (Proxy :: Proxy Account)
      , DefineElm (Proxy :: Proxy Session)
      , DefineElm (Proxy :: Proxy Token)
      , DefineElm (Proxy :: Proxy SignUp)
      ]
    proxyApi =
      (Proxy :: Proxy (RestApi '[Cookie]))
  in
    generateElmModuleWith options namespace defElmImports targetFolder elmDefinitions proxyApi
