{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm
import qualified Data.Aeson as Aeson
import RequestCollection
import RequestNode.Model
import Environment.App
import Session.Model
import Account.Model
import Model
import Http
import AppHealth
import App
import ElmOption (deriveElmDefOption)
import Servant.API.ContentTypes (NoContent)
import Servant.Auth.Server (JWT)
import Servant ((:<|>))

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

main :: IO ()
main =
  let
    options :: ElmOptions
    options = defElmOptions { urlPrefix = Dynamic }
    namespace =
      [ "Api"
      , "Client"
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
      ]
    proxyApi = (Proxy :: Proxy (ProtectedApi :<|> SessionApi))
  in
    generateElmModuleWith options namespace defElmImports targetFolder elmDefinitions proxyApi
