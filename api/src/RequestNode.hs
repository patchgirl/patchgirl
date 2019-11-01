{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RequestNode where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Servant (Handler)
import           Control.Monad.IO.Class (liftIO)
import           DB
import Http
import           Data.Aeson (FromJSON, ToJSON)
import RequestNode.Model

-- * file

insertRequestFile :: NewRequestFile -> Connection -> IO Int
insertRequestFile newRequestFile connection = do
  [Only id] <- query connection rawQuery $ newRequestFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name,
            http_method
          )
          VALUES (?, ?, ?, ?, ?)
          RETURNING id
          |]

createRequestFile :: Int -> NewRequestFile -> Handler Int
createRequestFile requestCollectionId newRequestFile = do
  liftIO (getDBConnection >>= (insertRequestFile newRequestFile)) >>= return

updateRequestFile :: Int -> Int -> Handler Int
updateRequestFile requestCollectionId requestFileId =
  undefined

-- * folder

insertRequestFolder :: NewRequestFolder -> Connection -> IO Int
insertRequestFolder newRequestFolder connection = do
  [Only id] <- query connection rawQuery $ newRequestFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, ?, ?)
          RETURNING id
          |]
