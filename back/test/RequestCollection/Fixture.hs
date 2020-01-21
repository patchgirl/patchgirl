{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module RequestCollection.Fixture where

import           Data.Aeson            (Value)
import           Data.Aeson.QQ
import           Http
import           RequestCollection.App
import           RequestNode.Model

requestCollectionSample1 :: RequestCollection
requestCollectionSample1 =
  RequestCollection 1 requestNodesTopLevel
  where
    requestNodesTopLevel =
      [ RequestFile { _requestNodeId = 1
                    , _requestNodeName = "someRequest1"
                    , _requestNodeHttpUrl = "someUrl1"
                    , _requestNodeHttpMethod = Get
                    , _requestNodeHttpHeaders = []
                    , _requestNodeHttpBody = ""
                    }
      , RequestFolder { _requestNodeId = 0
                      , _requestNodeName = "someFolder1"
                      , _requestNodeChildren = requestNodesSubLevel
                      }
      ]
    requestNodesSubLevel =
      [ RequestFile { _requestNodeId = 2
                    , _requestNodeName = "someRequest2"
                    , _requestNodeHttpUrl = "someUrl2"
                    , _requestNodeHttpMethod = Get
                    , _requestNodeHttpHeaders = []
                    , _requestNodeHttpBody = ""
                    }
      ]

requestCollectionSample2AsValue :: Value
requestCollectionSample2AsValue =
  [aesonQQ|
          [
            {
                "tag": "RequestFolder",
                "name": "2/"
            },
            {
                "tag": "RequestFolder",
                "name": "1/",
                "children": {
                    "tag": "RequestFolder",
                    "name": "1.1/",
                    "children": [
                        {
                            "tag": "RequestFile",
                            "name": "1.1.1",
                            "http_url": "api.com",
                            "http_body": "",
                            "http_method": "Get",
                            "http_headers": ""
                        }
                    ]
                }
            }
          ]
          |]

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
