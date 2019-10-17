{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy),
                               defaultOptions, deriveElmDef, defElmImports, defElmOptions,
                               deriveBoth, generateElmModuleWith)
import RequestCollection
import App

deriveElmDef defaultOptions ''RequestCollection
deriveElmDef defaultOptions ''RequestNode

main :: IO ()
main =
  let
    options = defElmOptions
    namespace = [ "Client" ]
    targetFolder = "../front/elm"
    elmDefinitions = [ DefineElm (Proxy :: Proxy RequestCollection)
                     , DefineElm (Proxy :: Proxy RequestNode)
                     ]
    proxyApi = (Proxy :: Proxy Api)
  in
    generateElmModuleWith options namespace defElmImports targetFolder elmDefinitions proxyApi
