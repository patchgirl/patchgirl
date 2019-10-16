{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RequestCollection.Fixture where

import RequestCollection
import NeatInterpolation
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import           Data.Aeson (decode, encode, Value)
import Data.Maybe (fromJust)

requestCollectionSample1 :: RequestCollection
requestCollectionSample1 =
  RequestCollection 1 requestNodesTopLevel
  where
    requestNodesTopLevel =
      [ Request2 { name = "someRequest1", url = "someUrl1" }
      , RequestFolder { name = "someFolder1"
                      , children = requestNodesSubLevel
                      }
      ]
    requestNodesSubLevel =
      [
        Request2 { name = "someRequest2", url = "someUrl2" }
      ]

requestNodesSample1AsValue :: Value
requestNodesSample1AsValue =
  let RequestCollection _ requestNodes = requestCollectionSample1
  in
    fromJust $ decode $ encode requestNodes


requestColectionSample1AsValue :: Value
requestColectionSample1AsValue =
  fromJust $ decode $ encodeUtf8 requestCollectionSample1AsText

requestCollectionSample1AsText :: Text
requestCollectionSample1AsText =
  fromStrict $ [text|
       [
         1,
         [
           {
              "tag": "Request2",
              "name": "someRequest1",
              "url": "someUrl1"
           },
           {
              "tag": "RequestFolder",
              "name": "someFolder1",
              "children": [
                 {
                    "tag": "Request2",
                    "name": "someRequest2",
                    "url": "someUrl2"
                 }
              ]
           }
         ]
       ]
       |]
