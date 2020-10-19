{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}

module DBUtil where

import qualified Data.ByteString                        as BS
import qualified Data.ByteString.UTF8                   as BSU
import           Data.Function                          ((&))
import           Data.Functor                           ((<&>))
import qualified Data.Map.Strict                        as Map
import qualified Data.Maybe                             as Maybe
import qualified Data.Strings                           as Strings
import           Data.UUID
import qualified Data.UUID.V4                           as UUID
import qualified Database.PostgreSQL.Simple             as PG
import qualified Database.PostgreSQL.Simple.FromField   as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types       as PG

import           GHC.Generics

import           PatchGirl.Web.CaseInsensitive
import           PatchGirl.Web.Environment.Model
import           PatchGirl.Web.Http
import           PatchGirl.Web.PgCollection.Model
import           PatchGirl.Web.PgNode.Model
import           PatchGirl.Web.PgNode.Sql
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.RequestNode.Sql
import           PatchGirl.Web.ScenarioCollection.Model
import           PatchGirl.Web.ScenarioNode.Model
import           PatchGirl.Web.ScenarioNode.Sql


-- * account


-- ** new fake account


defaultNewFakeAccount1 :: Int
defaultNewFakeAccount1 =
  1

defaultNewFakeAccount2 :: Int
defaultNewFakeAccount2 =
  2

insertFakeAccount :: Int -> PG.Connection -> IO UUID
insertFakeAccount githubId connection = do
  [PG.Only id] <- PG.query connection rawQuery (PG.Only githubId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO account (github_id)
          VALUES (?)
          RETURNING id
          |]


-- ** fake account without password


newtype NewFakeAccountWithoutPassword =
  NewFakeAccountWithoutPassword { _newFakeAccountWithoutPasswordEmail :: CaseInsensitive }
  deriving (Eq, Show, Read, Generic, PG.ToRow)

insertFakeAccountWithoutPassword :: NewFakeAccountWithoutPassword -> PG.Connection -> IO (UUID, String)
insertFakeAccountWithoutPassword newFakeAccount connection = do
  [(id, signupToken)] <- PG.query connection rawQuery newFakeAccount
  return (id, signupToken)
  where
    rawQuery =
      [sql|
          INSERT INTO account (email) values (?)
          RETURNING id, signup_token
          |]

-- ** fake account


data FakeAccount = FakeAccount
    { _fakeAccountId    :: UUID
    , _fakeAccountEmail :: Maybe CaseInsensitive
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

selectFakeAccount :: UUID -> PG.Connection -> IO (Maybe FakeAccount)
selectFakeAccount id connection =
  PG.query connection rawQuery (PG.Only id) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT id, email
          FROM account
          WHERE id = ?
          |]


-- * request collection


-- ** new fake request collection


insertFakeRequestCollection :: UUID -> PG.Connection -> IO Int
insertFakeRequestCollection accountId connection = do
  [PG.Only id] <- PG.query connection rawQuery (PG.Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection (account_id)
          VALUES (?)
          RETURNING id
          |]


-- ** insert request collection to request node


data NewFakeRequestCollectionToRequestNode = NewFakeRequestCollectionToRequestNode
    { _fakeRequestCollectionToRequestNodeRequestCollectionId  :: Int
    , _fakeRequestCollectionToRequestNodeRequestRequestNodeId :: UUID
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

insertFakeRequestCollectionToRequestNode :: NewFakeRequestCollectionToRequestNode -> PG.Connection -> IO ()
insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode connection = do
  _ <- PG.execute connection rawQuery fakeRequestCollectionToRequestNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (?, ?)
          |]


-- ** insert fake request folder


data NewFakeRequestFolder = NewFakeRequestFolder
    { _newFakeRequestFolderId       :: UUID
    , _newFakeRequestFolderParentId :: Maybe UUID
    , _newFakeRequestName           :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakeRequestFolder :: NewFakeRequestFolder -> PG.Connection -> IO UUID
insertFakeRequestFolder fakeRequestFolder connection = do
  [PG.Only id] <- PG.query connection rawQuery fakeRequestFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (id, request_node_parent_id, tag, name)
          VALUES (?, ?, 'Folder', ?)
          RETURNING id;
          |]


-- ** insert fake request file


data NewFakeRequestFile = NewFakeRequestFile
    { _newFakeRequestFileId         :: UUID
    , _newFakeRequestFileParentId   :: Maybe UUID
    , _newFakeRequestFileName       :: String
    , _newFakeRequestFileHttpUrl    :: String
    , _newFakeRequestFileHttpMethod :: String
    , _newFakeRequestFileHttpBody   :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakeRequestFile :: NewFakeRequestFile -> PG.Connection -> IO UUID
insertFakeRequestFile newFakeRequestFile connection = do
  [PG.Only id] <- PG.query connection rawQuery newFakeRequestFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            id,
            request_node_parent_id,
            tag,
            name,
            http_url,
            http_method,
            http_body,
            http_headers
          ) VALUES (?, ?, 'File', ?,?,?,?, '{}')
          RETURNING id;
          |]


-- ** insert sample request collection


{-
  insert a collection that looks like this:


       1     2
      / \
     3   4
    / \
   5   6


-}

insertSampleRequestCollection :: UUID -> PG.Connection -> IO RequestCollection
insertSampleRequestCollection accountId connection = do
  n1Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n3 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n4 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n5 id n3Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n6 id n3Id) connection
  requestCollectionId <- insertFakeRequestCollection accountId connection
  let fakeRequestCollectionToRequestNode1 =
        NewFakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                              , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n1Id
                                              }
  let fakeRequestCollectionToRequestNode2 =
        NewFakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                              , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n2Id
                                              }

  _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode1 connection
  _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode2 connection
  requestNodes <- selectRequestNodesFromRequestCollectionId requestCollectionId connection
  return $
    RequestCollection requestCollectionId requestNodes

  where

-- *** level 1

    n1 id = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                 , _newFakeRequestFolderParentId = Nothing
                                 , _newFakeRequestName           = "1/"
                                 }

    n2 id = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                 , _newFakeRequestFolderParentId = Nothing
                                 , _newFakeRequestName           = "2/"
                                 }
-- *** level 2

    n3 id parentId = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                          , _newFakeRequestFolderParentId = Just parentId
                                          , _newFakeRequestName           = "3/"
                                          }

    n4 id parentId = NewFakeRequestFile { _newFakeRequestFileId = id
                                        , _newFakeRequestFileParentId = Just parentId
                                        , _newFakeRequestFileName       = "4"
                                        , _newFakeRequestFileHttpUrl    = "http://4.com"
                                        , _newFakeRequestFileHttpMethod = "Get"
                                        , _newFakeRequestFileHttpBody   = ""
                                        }

-- *** level 3

    n5 id parentId = NewFakeRequestFile { _newFakeRequestFileId = id
                                        , _newFakeRequestFileParentId = Just parentId
                                        , _newFakeRequestFileName       = "5"
                                        , _newFakeRequestFileHttpUrl    = "http://5.com"
                                        , _newFakeRequestFileHttpMethod = "Get"
                                        , _newFakeRequestFileHttpBody   = ""
                                        }

    n6 id parentId = NewFakeRequestFile { _newFakeRequestFileId = id
                                        , _newFakeRequestFileParentId = Just parentId
                                        , _newFakeRequestFileName       = "6"
                                        , _newFakeRequestFileHttpUrl    = "http://6.com"
                                        , _newFakeRequestFileHttpMethod = "Get"
                                        , _newFakeRequestFileHttpBody   = ""
                                        }



-- * pg collection


-- ** new fake pg collection


insertFakePgCollection :: UUID -> PG.Connection -> IO UUID
insertFakePgCollection accountId connection = do
  [PG.Only id] <- PG.query connection rawQuery (PG.Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_collection (account_id)
          VALUES (?)
          RETURNING id
          |]


-- ** insert pg collection to pg node


data NewFakePgCollectionToPgNode = NewFakePgCollectionToPgNode
    { _fakePgCollectionToPgNodePgCollectionId :: UUID
    , _fakePgCollectionToPgNodePgPgNodeId     :: UUID
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

insertFakePgCollectionToPgNode :: NewFakePgCollectionToPgNode -> PG.Connection -> IO ()
insertFakePgCollectionToPgNode fakePgCollectionToPgNode connection = do
  _ <- PG.execute connection rawQuery fakePgCollectionToPgNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_actor_id)
          VALUES (?, ?)
          |]


-- ** insert fake pg folder


data NewFakePgFolder = NewFakePgFolder
    { _newFakePgFolderId       :: UUID
    , _newFakePgFolderParentId :: Maybe UUID
    , _newFakePgName           :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakePgFolder :: NewFakePgFolder -> PG.Connection -> IO UUID
insertFakePgFolder fakePgFolder connection = do
  [PG.Only id] <- PG.query connection rawQuery fakePgFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
          VALUES (?, ?, 'Folder', ?)
          RETURNING id;
          |]


-- ** insert fake pg file


data NewFakePgFile = NewFakePgFile
    { _newFakePgFileId       :: UUID
    , _newFakePgFileParentId :: Maybe UUID
    , _newFakePgFileName     :: String
    , _newFakePgFileSql      :: String
    , _newFakePgFileHost     :: String
    , _newFakePgFilePassword :: String
    , _newFakePgFilePort     :: String
    , _newFakePgFileUser     :: String
    , _newFakePgFileDbName   :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakePgFile :: NewFakePgFile -> PG.Connection -> IO UUID
insertFakePgFile newFakePgFile connection = do
  [PG.Only id] <- PG.query connection rawQuery newFakePgFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (
            tag,
            id,
            pg_node_parent_id,
            name,
            sql,
            pg_host,
            pg_password,
            pg_port,
            pg_user,
            pg_dbname
          ) VALUES ('File', ?, ?, ?, ?, ?, ?, ?, ?, ?)
          RETURNING id;
          |]


-- ** insert sample pg collection


{-
  insert a collection that looks like this:


       1     2
      / \
     3   4
    / \
   5   6


-}

insertSamplePgCollection :: UUID -> PG.Connection -> IO PgCollection
insertSamplePgCollection accountId connection = do
  n1Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n3 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakePgFile (n4 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakePgFile (n5 id n3Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakePgFile (n6 id n3Id) connection
  pgCollectionId <- insertFakePgCollection accountId connection
  let fakePgCollectionToPgNode1 =
        NewFakePgCollectionToPgNode { _fakePgCollectionToPgNodePgCollectionId = pgCollectionId
                                    , _fakePgCollectionToPgNodePgPgNodeId = n1Id
                                    }
  let fakePgCollectionToPgNode2 =
        NewFakePgCollectionToPgNode { _fakePgCollectionToPgNodePgCollectionId = pgCollectionId
                                              , _fakePgCollectionToPgNodePgPgNodeId = n2Id
                                              }
  _ <- insertFakePgCollectionToPgNode fakePgCollectionToPgNode1 connection
  _ <- insertFakePgCollectionToPgNode fakePgCollectionToPgNode2 connection
  pgNodes <- selectPgNodesFromPgCollectionId pgCollectionId connection
  return $
    PgCollection pgCollectionId pgNodes

  where

-- *** level 1

    n1 id = NewFakePgFolder { _newFakePgFolderId = id
                            , _newFakePgFolderParentId = Nothing
                            , _newFakePgName           = "1/"
                            }

    n2 id = NewFakePgFolder { _newFakePgFolderId = id
                            , _newFakePgFolderParentId = Nothing
                            , _newFakePgName           = "2/"
                            }
-- *** level 2

    n3 id parentId = NewFakePgFolder { _newFakePgFolderId = id
                                     , _newFakePgFolderParentId = Just parentId
                                     , _newFakePgName           = "3/"
                                     }

    n4 id parentId = NewFakePgFile { _newFakePgFileId = id
                                   , _newFakePgFileParentId = Just parentId
                                   , _newFakePgFileName = "4"
                                   , _newFakePgFileSql = ""
                                   , _newFakePgFileHost = ""
                                   , _newFakePgFilePassword = ""
                                   , _newFakePgFilePort = ""
                                   , _newFakePgFileUser = ""
                                   , _newFakePgFileDbName = ""
                                   }

-- *** level 3

    n5 id parentId = NewFakePgFile { _newFakePgFileId = id
                                   , _newFakePgFileParentId = Just parentId
                                   , _newFakePgFileName       = "5"
                                   , _newFakePgFileSql = ""
                                   , _newFakePgFileHost = ""
                                   , _newFakePgFilePassword = ""
                                   , _newFakePgFilePort = ""
                                   , _newFakePgFileUser = ""
                                   , _newFakePgFileDbName = ""
                                   }

    n6 id parentId = NewFakePgFile { _newFakePgFileId = id
                                   , _newFakePgFileParentId = Just parentId
                                   , _newFakePgFileName       = "6"
                                   , _newFakePgFileSql = ""
                                   , _newFakePgFileHost = ""
                                   , _newFakePgFilePassword = ""
                                   , _newFakePgFilePort = ""
                                   , _newFakePgFileUser = ""
                                   , _newFakePgFileDbName = ""
                                   }


-- * request node


-- ** select fake request file


data FakeRequestFile = FakeRequestFile
    { _fakeRequestFileParentId    :: Maybe UUID
    , _fakeRequestFileName        :: String
    , _fakeRequestFileHttpUrl     :: String
    , _fakeRequestFileHttpMethod  :: Method
    , _fakeRequestFileHttpHeaders :: HttpHeaders
    , _fakeRequestFileHttpBody    :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

newtype HttpHeaders = HttpHeaders [Header] deriving (Eq, Show, Read, Generic)
newtype Header = Header (String, String) deriving (Eq, Show, Read, Generic)

instance PG.FromField HttpHeaders where
  fromField field mdata = do
    PG.PGArray httpHeaders <- PG.fromField field mdata :: PG.Conversion (PG.PGArray Header)
    return $ HttpHeaders httpHeaders

{-
  this instance is only for the tests, it will fail for sure with real data
  dont use in prod !!!
  it will fail if either the header key or header value contains a (,) or (") for example
-}
instance PG.FromField Header where
  fromField _ = \case
    Nothing -> error "invalid field"
    Just bs ->
      return $ readHttpHeader bs
    where
      readHttpHeader :: BS.ByteString -> Header
      readHttpHeader bs = -- bs should have the shape: (someHeader,someValue)
        let
          (key, value) = Strings.strSplit "," $ init $ tail $ BSU.toString bs :: (String, String)
        in Header (key, value)

selectFakeRequestFile :: UUID -> PG.Connection -> IO FakeRequestFile
selectFakeRequestFile id connection = do
  [fakeRequestFile] <- PG.query connection rawQuery (PG.Only id)
  return fakeRequestFile
  where
    rawQuery =
      [sql|
          SELECT request_node_parent_id, name, http_url, http_method, http_headers, http_body
          FROM request_node
          where id = ?
          |]


-- ** select fake request folder


data FakeRequestFolder = FakeRequestFolder
    { _fakeRequestFolderParentId :: Maybe UUID
    , _fakeRequestFolderName     :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)


selectFakeRequestFolder :: UUID -> PG.Connection -> IO FakeRequestFolder
selectFakeRequestFolder fakeRequestFolderId connection = do
  [fakeRequestFolder] <- PG.query connection rawQuery (PG.Only fakeRequestFolderId)
  return fakeRequestFolder
  where
    rawQuery =
      [sql|
          SELECT request_node_parent_id, name
          FROM request_node
          where id = ?
          |]


-- ** select request node exist


selectRequestNodeExists :: UUID -> PG.Connection -> IO Bool
selectRequestNodeExists id connection = do
  [PG.Only nodeExists] <- PG.query connection rawQuery (PG.Only id)
  return nodeExists
  where
    rawQuery =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_node
            WHERE id = ?
          )
          |]



-- * pg node


-- ** select fake pg file


data FakePgFile = FakePgFile
    { _fakePgFileParentId :: Maybe UUID
    , _fakePgFileName     :: String
    , _fakePgFileSql      :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

selectFakePgFile :: UUID -> PG.Connection -> IO FakePgFile
selectFakePgFile id connection = do
  [fakePgFile] <- PG.query connection rawQuery (PG.Only id)
  return fakePgFile
  where
    rawQuery =
      [sql|
          SELECT pg_node_parent_id, name, sql
          FROM pg_node
          where id = ?
          |]


-- ** select fake pg folder


data FakePgFolder = FakePgFolder
    { _fakePgFolderParentId :: Maybe UUID
    , _fakePgFolderName     :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)


selectFakePgFolder :: UUID -> PG.Connection -> IO FakePgFolder
selectFakePgFolder fakePgFolderId connection = do
  [fakePgFolder] <- PG.query connection rawQuery (PG.Only fakePgFolderId)
  return fakePgFolder
  where
    rawQuery =
      [sql|
          SELECT pg_node_parent_id, name
          FROM pg_node
          where id = ?
          |]


-- ** select pg node exist


selectPgNodeExists :: UUID -> PG.Connection -> IO Bool
selectPgNodeExists id connection = do
  [PG.Only nodeExists] <- PG.query connection rawQuery (PG.Only id)
  return nodeExists
  where
    rawQuery =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM pg_node
            WHERE id = ?
          )
          |]



-- * scenario collection


-- ** new fake scenario collection


insertFakeScenarioCollection :: UUID -> PG.Connection -> IO UUID
insertFakeScenarioCollection accountId connection = do
  [PG.Only id] <- PG.query connection rawQuery (PG.Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_collection (id, account_id)
          VALUES (uuid_generate_v4(), ?)
          RETURNING id
          |]


-- ** insert scenario collection to scenario node


data NewFakeScenarioCollectionToScenarioNode = NewFakeScenarioCollectionToScenarioNode
    { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId :: UUID
    , _fakeScenarioCollectionToScenarioNodeScenarioNodeId       :: UUID
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

insertFakeScenarioCollectionToScenarioNode :: NewFakeScenarioCollectionToScenarioNode -> PG.Connection -> IO ()
insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode connection = do
  _ <- PG.execute connection rawQuery fakeScenarioCollectionToScenarioNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_collection_to_scenario_node (scenario_collection_id, scenario_node_id)
          VALUES (?, ?)
          |]


-- ** insert fake scenario folder


data NewFakeScenarioFolder = NewFakeScenarioFolder
    { _newFakeScenarioFolderId       :: UUID
    , _newFakeScenarioFolderParentId :: Maybe UUID
    , _newFakeScenarioName           :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakeScenarioFolder :: NewFakeScenarioFolder -> PG.Connection -> IO UUID
insertFakeScenarioFolder fakeScenarioFolder connection = do
  [PG.Only id] <- PG.query connection rawQuery fakeScenarioFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name)
          VALUES (?, ?, 'Folder', ?)
          RETURNING id;
          |]


-- ** insert fake scenario file


data NewFakeScenarioFile = NewFakeScenarioFile
    { _newFakeScenarioFileId            :: UUID
    , _newFakeScenarioFileParentId      :: Maybe UUID
    , _newFakeScenarioFileName          :: String
    , _newFakeScenarioFileSceneId       :: Maybe UUID
    , _newFakeScenarioFileEnvironmentId :: UUID
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)


insertFakeScenarioFile :: NewFakeScenarioFile -> PG.Connection -> IO UUID
insertFakeScenarioFile newFakeScenarioFile connection = do
  [PG.Only id] <- PG.query connection rawQuery newFakeScenarioFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (
            id,
            scenario_node_parent_id,
            tag,
            name,
            scene_node_id,
            environment_id
          ) VALUES (?, ?, 'File', ?,?,?)
          RETURNING id;
          |]


-- ** insert sample scenario collection


{-
  insert a collection that looks like this:


                     1/     2/
                    /
                   3/
                  / \
                 5   6
                 |
               scene1
-}

insertSampleScenarioCollection :: UUID -> PG.Connection -> IO (RequestCollection, ScenarioCollection)
insertSampleScenarioCollection accountId connection = do


-- *** insert request collection


  requestCollection@(RequestCollection _ requestNodes) <- insertSampleRequestCollection accountId connection
  let newFakeEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                             , _newFakeEnvironmentName      = "env1"
                                             }
  _ <- insertNewFakeEnvironment newFakeEnvironment connection
  let requestFileId = (Maybe.fromJust . getFirstFile) requestNodes & _requestNodeId
  let newFakeScene =
        NewFakeHttpScene { _newFakeSceneParentId = Nothing
                         , _newFakeSceneRequestId = requestFileId
                         , _newFakeSceneVariables = SceneVariables Map.empty
                         , _newFakeScenePrescript = ""
                         , _newFakeScenePostscript = ""
                         }
  scene1Id <- insertFakeHttpScene newFakeScene connection


-- *** insert pg collection


  PgCollection _ pgNodes <- insertSamplePgCollection accountId connection
  let newFakeEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                             , _newFakeEnvironmentName      = "env1"
                                             }
  envId <- insertNewFakeEnvironment newFakeEnvironment connection
  let pgFileId = (Maybe.fromJust . getFirstPgFile) pgNodes & _pgNodeId
  let newFakeScene =
        NewFakePgScene { _newFakePgSceneParentId = Nothing
                       , _newFakePgSceneActorId = pgFileId
                       , _newFakePgSceneVariables = SceneVariables Map.empty
                       , _newFakePgScenePrescript = ""
                       , _newFakePgScenePostscript = ""
                       }
  scene2Id <- insertFakePgScene newFakeScene connection


-- *** insert scenario collection


  n1Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n3 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeScenarioFile (n5 id n3Id (Just scene1Id) envId) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeScenarioFile (n6 id n3Id (Just scene2Id) envId) connection
  scenarioCollectionId <- insertFakeScenarioCollection accountId connection
  let fakeScenarioCollectionToScenarioNode1 =
        NewFakeScenarioCollectionToScenarioNode { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId = scenarioCollectionId
                                                , _fakeScenarioCollectionToScenarioNodeScenarioNodeId = n1Id
                                                }
  let fakeScenarioCollectionToScenarioNode2 =
        NewFakeScenarioCollectionToScenarioNode { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId = scenarioCollectionId
                                                , _fakeScenarioCollectionToScenarioNodeScenarioNodeId = n2Id
                                                }
  _ <- insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode1 connection
  _ <- insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode2 connection
  scenarioNodes <- selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection

  return (requestCollection, ScenarioCollection scenarioCollectionId scenarioNodes)

  where


-- *** level 1

    n1 id =
      NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                            , _newFakeScenarioFolderParentId = Nothing
                            , _newFakeScenarioName           = "1/"
                            }

    n2 id =
      NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                            , _newFakeScenarioFolderParentId = Nothing
                            , _newFakeScenarioName           = "2/"
                            }


-- *** level 2


    n3 id parentId =
      NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                            , _newFakeScenarioFolderParentId = Just parentId
                            , _newFakeScenarioName           = "3/"
                            }


-- *** level 3


    n5 id parentId sceneId environmentId =
      NewFakeScenarioFile { _newFakeScenarioFileId = id
                          , _newFakeScenarioFileParentId = Just parentId
                          , _newFakeScenarioFileName = "5"
                          , _newFakeScenarioFileSceneId  = sceneId
                          , _newFakeScenarioFileEnvironmentId = environmentId
                          }

    n6 id parentId sceneId environmentId =
      NewFakeScenarioFile { _newFakeScenarioFileId = id
                          , _newFakeScenarioFileParentId = Just parentId
                          , _newFakeScenarioFileName = "6"
                          , _newFakeScenarioFileSceneId = sceneId
                          , _newFakeScenarioFileEnvironmentId = environmentId
                          }


-- * scenario node


-- ** select fake scenario file


data FakeScenarioFile = FakeScenarioFile
    { _fakeScenarioFileParentId      :: Maybe UUID
    , _fakeScenarioFileName          :: String
    , _fakeScenarioFileSceneActorId  :: Maybe UUID
    , _fakeScenarioFileEnvironmentId :: Maybe UUID
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)


selectFakeScenarioFile :: UUID -> PG.Connection -> IO FakeScenarioFile
selectFakeScenarioFile id connection = do
  [fakeScenarioFile] <- PG.query connection rawQuery (PG.Only id)
  return fakeScenarioFile
  where
    rawQuery =
      [sql|
          SELECT scenario_node_parent_id, name, scene_node_id, environment_id
          FROM scenario_node
          WHERE id = ?
          AND tag = 'File'
          |]


-- ** select fake scenario folder


data FakeScenarioFolder = FakeScenarioFolder
    { _fakeScenarioFolderParentId :: Maybe UUID
    , _fakeScenarioFolderName     :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)


selectFakeScenarioFolder :: UUID -> PG.Connection -> IO FakeScenarioFolder
selectFakeScenarioFolder scenarioNodeId connection = do
  [fakeScenarioFolder] <- PG.query connection rawQuery (PG.Only scenarioNodeId)
  return fakeScenarioFolder
  where
    rawQuery =
      [sql|
          SELECT scenario_node_parent_id, name
          FROM scenario_node
          where id = ?
          |]


-- ** select scenario node exist


selectScenarioNodeExists :: UUID -> PG.Connection -> IO Bool
selectScenarioNodeExists id connection = do
  [PG.Only nodeExists] <- PG.query connection rawQuery (PG.Only id)
  return nodeExists
  where
    rawQuery =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM scenario_node
            WHERE id = ?
          )
          |]


-- * scene

-- ** insert fake http scene


data NewFakeHttpScene = NewFakeHttpScene
    { _newFakeSceneParentId   :: Maybe UUID
    , _newFakeSceneRequestId  :: UUID
    , _newFakeSceneVariables  :: SceneVariables
    , _newFakeScenePrescript  :: String
    , _newFakeScenePostscript :: String
    }
    deriving (Eq, Show, Generic, PG.ToRow)


insertFakeHttpScene :: NewFakeHttpScene -> PG.Connection -> IO UUID
insertFakeHttpScene newFakeScene connection = do
  [PG.Only id] <- PG.query connection rawQuery newFakeScene
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scene_node(
            id,
            actor_type,
            pg_actor_id,
            scene_node_parent_id,
            http_actor_id,
            variables,
            prescript,
            postscript
          ) VALUES (gen_random_uuid(), 'HttpActor', NULL, ?, ?, ?, ?, ?)
          RETURNING id;
          |]


-- ** insert fake pg scene


data NewFakePgScene = NewFakePgScene
    { _newFakePgSceneParentId   :: Maybe UUID
    , _newFakePgSceneActorId    :: UUID
    , _newFakePgSceneVariables  :: SceneVariables
    , _newFakePgScenePrescript  :: String
    , _newFakePgScenePostscript :: String
    }
    deriving (Eq, Show, Generic, PG.ToRow)


insertFakePgScene :: NewFakePgScene -> PG.Connection -> IO UUID
insertFakePgScene newFakeScene connection = do
  [PG.Only id] <- PG.query connection rawQuery newFakeScene
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scene_node(
            id,
            actor_type,
            http_actor_id,
            scene_node_parent_id,
            pg_actor_id,
            variables,
            prescript,
            postscript
          ) VALUES (gen_random_uuid(), 'PgActor', NULL, ?, ?, ?, ?, ?)
          RETURNING id;
          |]


-- ** select fake scene


data FakeScene = FakeScene
    { _fakeSceneParentId   :: Maybe UUID
    , _fakeActorType       :: ActorType
    , _fakeSceneId         :: UUID
    , _fakeSceneVariables  :: SceneVariables
    , _fakeScenePrescript  :: String
    , _fakeScenePostscript :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow)

selectFakeScene :: UUID -> PG.Connection -> IO (Maybe FakeScene)
selectFakeScene id connection =
  PG.query connection rawQuery (PG.Only id) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT
            scene_node_parent_id,
            actor_type,
            (CASE
              WHEN actor_type = 'HttpActor' THEN http_actor_id
              WHEN actor_type = 'PgActor' THEN pg_actor_id
              ELSE NULL
              END
            ),
            variables,
            prescript,
            postscript
          FROM scene_node
          WHERE id = ?
          |]

selectFakeSceneWithParentId :: UUID -> PG.Connection -> IO (Maybe FakeScene)
selectFakeSceneWithParentId parentId connection =
  PG.query connection rawQuery (PG.Only parentId) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT
            scene_node_parent_id,
            actor_type request_node_id,
            (CASE
              WHEN actor_type = 'HttpActor' THEN http_actor_id
              WHEN actor_type = 'PgActor' THEN pg_actor_id
              ELSE NULL
              END
            ),
            variables,
            prescript,
            postscript
          FROM scene_node
          WHERE scene_node_parent_id = ?
          |]


-- * environment


-- ** insert fake environment


data NewFakeEnvironment = NewFakeEnvironment
    { _newFakeEnvironmentAccountId :: UUID
    , _newFakeEnvironmentName      :: String
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

insertNewFakeEnvironment :: NewFakeEnvironment -> PG.Connection -> IO UUID
insertNewFakeEnvironment NewFakeEnvironment { _newFakeEnvironmentAccountId, _newFakeEnvironmentName } connection = do
  [PG.Only fakeEnvironmentId] <- PG.query connection rawQuery (_newFakeEnvironmentName, _newFakeEnvironmentAccountId)
  return fakeEnvironmentId
  where
    rawQuery =
      [sql|
          WITH new_env AS (
            INSERT INTO environment (id, name)
            VALUES (gen_random_uuid(), ?)
            RETURNING id
          ), new_account_env AS (
            INSERT INTO account_environment (account_id, environment_id)
            VALUES (?, (SELECT id FROM new_env))
          ) SELECT id FROM new_env;
          |]


-- ** fake environment


data FakeEnvironment = FakeEnvironment
    { _fakeEnvironmentId   :: UUID
    , _fakeEnvironmentName :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

selectFakeEnvironment :: UUID -> PG.Connection -> IO (Maybe FakeEnvironment)
selectFakeEnvironment environmentId connection =
  PG.query connection rawQuery (PG.Only environmentId) <&> Maybe.listToMaybe
  where
    rawQuery =
      [sql|
          SELECT id, name
          FROM environment
          WHERE id = ?
          |]


-- ** fake account environment


data FakeAccountEnvironment = FakeAccountEnvironment
    { _fakeAccountEnvironmentAccountId     :: UUID
    , _fakeAccountEnvironmentEnvironmentId :: UUID
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

selectFakeAccountEnvironments :: UUID -> PG.Connection -> IO [FakeAccountEnvironment]
selectFakeAccountEnvironments accountId connection =
  PG.query connection rawQuery (PG.Only accountId)
  where
    rawQuery =
      [sql|
          SELECT account_id, environment_id
          FROM account_environment
          WHERE account_id = ?
          |]


-- ** insert fake environment key value


data NewFakeKeyValue = NewFakeKeyValue
    { _newFakeKeyValueEnvironmentId :: UUID
    , _newFakeKeyValueKey           :: String
    , _newFakeKeyValueValue         :: String
    , _newFakeKeyValueHidden        :: Bool
    }
    deriving (Eq, Show, Read, Generic, PG.ToRow)

insertNewFakeKeyValue :: NewFakeKeyValue -> PG.Connection -> IO KeyValue
insertNewFakeKeyValue newFakeKeyValue connection = do
  [keyValue] <- PG.query connection rawQuery newFakeKeyValue
  return keyValue
  where
    rawQuery =
      [sql|
          INSERT INTO key_value (id, environment_id, key, value, hidden)
          VALUES (gen_random_uuid(), ?, ?, ?, ?)
          RETURNING id, key, value, hidden;
          |]


-- ** select fake key value


data FakeKeyValue = FakeKeyValue
    { _fakeKeyValueId            :: UUID
    , _fakeKeyValueEnvironmentId :: UUID
    , _fakeKeyValueKey           :: String
    , _fakeKeyValueValue         :: String
    }
    deriving (Eq, Show, Read, Generic, PG.FromRow)

selectFakeKeyValues :: UUID -> PG.Connection -> IO [KeyValue]
selectFakeKeyValues environmentId connection =
  PG.query connection rawQuery (PG.Only environmentId)
  where
    rawQuery =
      [sql|
          SELECT id, key, value, hidden
          FROM key_value
          WHERE environment_id = ?
          |]


-- * util


-- ** scenario


getFirstScenarioFolder :: [ScenarioNode] -> Maybe ScenarioNode
getFirstScenarioFolder scenarioNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFolder scenarioNodes)
  where
    findFolder :: ScenarioNode -> Maybe ScenarioNode
    findFolder = \case
      folder@ScenarioFolder {} -> Just folder
      _ -> Nothing


getFirstScenarioFile :: [ScenarioNode] -> Maybe ScenarioNode
getFirstScenarioFile scenarioNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFile scenarioNodes)
  where
    findFile :: ScenarioNode -> Maybe ScenarioNode
    findFile = \case
      file@ScenarioFile {} -> Just file
      ScenarioFolder { _scenarioNodeChildren } ->
        getFirstScenarioFile _scenarioNodeChildren


-- ** request


getFirstFolder :: [RequestNode] -> Maybe RequestNode
getFirstFolder requestNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFolder requestNodes)
  where
    findFolder :: RequestNode -> Maybe RequestNode
    findFolder = \case
      folder@RequestFolder {} -> Just folder
      _ -> Nothing

getFirstFile :: [RequestNode] -> Maybe RequestNode
getFirstFile requestNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFile requestNodes)
  where
    findFile :: RequestNode -> Maybe RequestNode
    findFile = \case
      file@RequestFile {} -> Just file
      RequestFolder { _requestNodeChildren } ->
        getFirstFile _requestNodeChildren

-- ** pg


getFirstPgFolder :: [PgNode] -> Maybe PgNode
getFirstPgFolder pgNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFolder pgNodes)
  where
    findFolder :: PgNode -> Maybe PgNode
    findFolder = \case
      folder@PgFolder {} -> Just folder
      _ -> Nothing

getFirstPgFile :: [PgNode] -> Maybe PgNode
getFirstPgFile pgNodes =
  Maybe.listToMaybe (Maybe.mapMaybe findFile pgNodes)
  where
    findFile :: PgNode -> Maybe PgNode
    findFile = \case
      file@PgFile {} -> Just file
      PgFolder { _pgNodeChildren } ->
        getFirstPgFile _pgNodeChildren
