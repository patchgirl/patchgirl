{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RequestCollection.Fixture where

import RequestCollection
import Data.Aeson (Value)
import Data.Aeson.QQ
import Http

requestCollectionSample1 :: RequestCollection
requestCollectionSample1 =
  RequestCollection 1 requestNodesTopLevel
  where
    requestNodesTopLevel =
      [ RequestFile { name = "someRequest1"
                    , url = "someUrl1"
                    , method = Get
                    , headers = []
                    , body = ""
                    }
      , RequestFolder { name = "someFolder1"
                      , children = requestNodesSubLevel
                      }
      ]
    requestNodesSubLevel =
      [ RequestFile { name = "someRequest2"
                    , url = "someUrl2"
                    , method = Get
                    , headers = []
                    , body = ""
                    }
      ]

requestCollectionSample1AsValue :: Value
requestCollectionSample1AsValue =
  [aesonQQ|
          [ 1
          , [ {
                "tag": "RequestFile",
                "name": "someRequest1",
                "url": "someUrl1",
                "method": "Get",
                "headers": [],
                "body": ""
              }
            , {
                "tag": "RequestFolder",
                "name": "someFolder1",
                "children": [
                   {
                      "tag": "RequestFile",
                      "name": "someRequest2",
                      "url": "someUrl2",
                      "method": "Get",
                      "headers": [],
                      "body": ""
                   }
                ]
             }
           ]
         ]
         |]
