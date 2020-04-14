{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Scene.DB where

import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics


-- * insert fake scene


data NewFakeScene =
  NewFakeScene { _newFakeSceneParentId  :: Maybe UUID
               , _newFakeSceneRequestId :: UUID
               }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeScene :: NewFakeScene -> Connection -> IO UUID
insertFakeScene newFakeScene connection = do
  [Only id] <- query connection rawQuery newFakeScene
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scene_node(
            id,
            scene_node_parent_id,
            request_node_id
          ) VALUES (gen_random_uuid(), ?, ?)
          RETURNING id;
          |]


-- * select fake scene


data FakeScene =
  FakeScene { _fakeSceneParentId  :: Maybe UUID
            , _fakeSceneRequestId :: UUID
            }
  deriving (Eq, Show, Read, Generic, FromRow)

selectFakeScene :: UUID -> Connection -> IO (Maybe FakeScene)
selectFakeScene id connection =
  query connection rawQuery (Only id) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT scene_node_parent_id, request_node_id
          FROM scene_node
          WHERE id = ?
          |]

selectFakeSceneWithParentId :: UUID -> Connection -> IO (Maybe FakeScene)
selectFakeSceneWithParentId parentId connection =
  query connection rawQuery (Only parentId) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT scene_node_parent_id, request_node_id
          FROM scene_node
          WHERE scene_node_parent_id = ?
          |]
