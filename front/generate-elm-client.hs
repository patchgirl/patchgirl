{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm
import qualified Data.Aeson as Aeson
import RequestCollection
import AppHealth
import App
import ElmOption (deriveElmDefOption)

deriveElmDef deriveElmDefOption ''RequestCollection
deriveElmDef deriveElmDefOption ''RequestNode
deriveElmDef deriveElmDefOption ''AppHealth

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
      , DefineElm (Proxy :: Proxy AppHealth)
      ]
    proxyApi = (Proxy :: Proxy Api)
  in
    generateElmModuleWith options namespace defElmImports targetFolder elmDefinitions proxyApi
