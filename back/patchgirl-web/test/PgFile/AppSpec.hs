{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module PgFile.AppSpec where

import           Data.Function                    ((&))
import qualified Data.Maybe                       as Maybe
import           Data.UUID
import qualified Data.UUID                        as UUID
import qualified Network.HTTP.Types               as HTTP
import           Servant                          hiding (Header)
import qualified Servant.Auth.Client              as Auth
import qualified Servant.Auth.Server              as Auth
import           Servant.Client                   (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.PgCollection.Model
import           PatchGirl.Web.PgNode.Model
import           PatchGirl.Web.Server


-- * client


createPgFileHandler :: Auth.Token -> UUID -> NewPgFile -> ClientM ()
createRootPgFileHandler :: Auth.Token -> UUID -> NewRootPgFile -> ClientM ()
updatePgFileHandler :: Auth.Token -> UUID -> UUID -> UpdatePgFile -> ClientM ()
createPgFileHandler
  :<|> createRootPgFileHandler
  :<|> updatePgFileHandler =
  client (Proxy :: Proxy (PgFileApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create pg file


    describe "create a pg file" $ do
      it "returns 404 when pg collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newPgFile = mkNewPgFile UUID.nil UUID.nil
          try clientEnv (createPgFileHandler token UUID.nil newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when pg node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId _ <- insertSamplePgCollection accountId connection
          let newPgFile = mkNewPgFile UUID.nil UUID.nil
          try clientEnv (createPgFileHandler token pgCollectionId newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 500 when pg node parent exist but isn't a pg folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let fileId = Maybe.fromJust (getFirstPgFile pgNodes) & _pgNodeId
          let newPgFile = mkNewPgFile UUID.nil fileId
          try clientEnv (createPgFileHandler token pgCollectionId newPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the pg file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let folderId = Maybe.fromJust (getFirstPgFolder pgNodes) & _pgNodeId
          let newPgFile = mkNewPgFile UUID.nil folderId
          _ <- try clientEnv (createPgFileHandler token pgCollectionId newPgFile)
          fakePgFile <- selectFakePgFile UUID.nil connection
          fakePgFile `shouldBe`  FakePgFile { _fakePgFileParentId   = Just folderId
                                            , _fakePgFileName       = "name"
                                            , _fakePgFileSql = "sql"
                                            }

-- ** create root pg file


    describe "create a root pg file" $ do
      it "returns 404 when pg collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRootPgFile = mkNewRootPgFile UUID.nil
          try clientEnv (createRootPgFileHandler token UUID.nil newRootPgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the pg file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          pgCollectionId <- insertFakePgCollection accountId connection
          let newRootPgFile = mkNewRootPgFile UUID.nil
          _ <- try clientEnv (createRootPgFileHandler token pgCollectionId newRootPgFile)
          fakePgFile <- selectFakePgFile UUID.nil connection
          fakePgFile `shouldBe`  FakePgFile { _fakePgFileParentId   = Nothing
                                            , _fakePgFileName       = "name"
                                            , _fakePgFileSql = "sql"
                                            }


-- ** update pg file


    describe "update a pg file" $ do
      it "returns 404 when pg file doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let updatePgFile = mkUpdatePgFile
          try clientEnv (updatePgFileHandler token UUID.nil UUID.nil updatePgFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "update the pg file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          PgCollection pgCollectionId pgNodes <- insertSamplePgCollection accountId connection
          let PgFile {..} = Maybe.fromJust $ getFirstPgFile pgNodes
          _ <- try clientEnv (updatePgFileHandler token pgCollectionId _pgNodeId mkUpdatePgFile)
          FakePgFile{..} <- selectFakePgFile _pgNodeId connection
          _fakePgFileName `shouldBe` "new name"
          _fakePgFileSql `shouldBe` "new sql"


  where
    mkNewPgFile :: UUID -> UUID -> NewPgFile
    mkNewPgFile id parentId =
      NewPgFile { _newPgFileId = id
                , _newPgFileParentNodeId = parentId
                , _newPgFileName = "name"
                , _newPgFileSql = "sql"
                , _newPgFileHost = "host"
                , _newPgFilePassword = "password"
                , _newPgFilePort = "port"
                , _newPgFileUser = "user"
                , _newPgFileDbName = "dbname"
                }

    mkNewRootPgFile :: UUID -> NewRootPgFile
    mkNewRootPgFile id =
      NewRootPgFile { _newRootPgFileId = id
                    , _newRootPgFileName = "name"
                    , _newRootPgFileSql = "sql"
                    , _newRootPgFileHost = "host"
                    , _newRootPgFilePassword = "password"
                    , _newRootPgFilePort = "port"
                    , _newRootPgFileUser = "user"
                    , _newRootPgFileDbName = "dbname"
                    }

    mkUpdatePgFile :: UpdatePgFile
    mkUpdatePgFile =
      UpdatePgFile { _updatePgFileName = "new name"
                   , _updatePgFileSql  = "new sql"
                   , _updatePgFileHost  = "new host"
                   , _updatePgFilePassword  = "new password"
                   , _updatePgFilePort  = "new port"
                   , _updatePgFileUser  = "new user"
                   , _updatePgFileDbName  = "new dbname"
                   }
