{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy),
                               defaultOptions, deriveElmDef, defElmImports, defElmOptions,
                               deriveBoth, generateElmModuleWith, urlPrefix, UrlPrefix(..), ElmOptions)
import RequestCollection
import AppHealth
import App

deriveElmDef defaultOptions ''RequestCollection
deriveElmDef defaultOptions ''RequestNode
deriveElmDef defaultOptions ''AppHealth

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
    elmDefinitions = [ DefineElm (Proxy :: Proxy RequestCollection)
                     , DefineElm (Proxy :: Proxy RequestNode)
                     , DefineElm (Proxy :: Proxy AppHealth)
                     ]
    proxyApi = (Proxy :: Proxy Api)
  in
    generateElmModuleWith options namespace defElmImports targetFolder elmDefinitions proxyApi
