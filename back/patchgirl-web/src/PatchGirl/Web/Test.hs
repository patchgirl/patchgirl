{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PatchGirl.Web.Test where

import qualified Control.Monad                    as Monad
import qualified Control.Monad.Except             as Except
import qualified Control.Monad.IO.Class           as IO
import qualified Control.Monad.Reader             as Reader
import qualified Data.Aeson                       as Aeson
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics                     (Generic)
import qualified GHC.Int                          as Int
import qualified Servant
import           Servant.API.ContentTypes         (NoContent (..))
import           Servant.Server                   (ServerError)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Data.ByteString.Char8                as B


import           PatchGirl.Web.DB
import           PatchGirl.Web.Internal.Env


-- * model


-- ** user


data UserTest
  = UserTest { userTest_id        :: Int
             , userTest_firstname :: String
             , userTest_lastname  :: String
             }
  deriving (Eq, Show, Read, Generic, PG.FromRow)

instance Aeson.ToJSON UserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userTest_" :: String) }

instance Aeson.FromJSON UserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userTest_" :: String) }


-- ** new user


data NewUserTest
  = NewUserTest { newUserTest_firstname :: String
                , newUserTest_lastname  :: String
                }
  deriving (Eq, Show, Read, Generic, PG.ToRow)

instance Aeson.ToJSON NewUserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newUserTest_" :: String) }

instance Aeson.FromJSON NewUserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("newUserTest_" :: String) }


-- * handler


-- ** create user


createUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => NewUserTest
  -> m UserTest
createUserHandler newUser = do
  connection <- getDBConnection
  IO.liftIO $ insertNewUserTestSql newUser connection

insertNewUserTestSql :: NewUserTest -> PG.Connection -> IO UserTest
insertNewUserTestSql newUser connection = do
  [user] <- PG.query connection rawQuery newUser
  return user
  where
    rawQuery =
      [sql|
          INSERT INTO user_test (firstname, lastname, role)
          VALUES (?, ?, 'normal')
          RETURNING id, firstname, lastname
          |]


-- ** delete user


deleteUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> m ()
deleteUserHandler userId = do
  connection <- getDBConnection
  IO.liftIO . Monad.void $ deleteUserTestSql userId connection

deleteUserTestSql :: Int -> PG.Connection -> IO Int.Int64
deleteUserTestSql userId connection = do
  PG.execute connection rawQuery (PG.Only userId)
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET deleted_at = NOW()
          WHERE id = ?
          |]


-- ** show user


showUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> m UserTest
showUserHandler userId = do
  connection <- getDBConnection
  IO.liftIO $ showUserTestSql userId connection

showUserTestSql :: Int -> PG.Connection -> IO UserTest
showUserTestSql userId connection = do
  [user] <- PG.query connection rawQuery (PG.Only userId)
  return user
  where
    rawQuery =
      [sql|
          SELECT id, firstname, lastname
          FROM user_test
          WHERE id = ?
          |]


-- ** update user


data UpdateUserTest
  = UpdateUserTest { updateUserTest_firstname :: String
                   , updateUserTest_lastname  :: String
                   }
  deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON UpdateUserTest where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateUserTest_" :: String) }

instance Aeson.FromJSON UpdateUserTest where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("updateUserTest_" :: String) }

updateUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Int
  -> UpdateUserTest
  -> m UserTest
updateUserHandler userId updateUser = do
  connection <- getDBConnection
  IO.liftIO $ updateUserTestSql userId updateUser connection

updateUserTestSql :: Int -> UpdateUserTest -> PG.Connection -> IO UserTest
updateUserTestSql userId UpdateUserTest{..} connection = do
  [user] <- PG.query connection rawQuery ( updateUserTest_firstname
                                         , updateUserTest_lastname
                                         , userId
                                         )
  return user
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET firstname = ?, lastname = ?
          WHERE id = ?
          RETURNING id, firstname, lastname
          |]


-- ** update role


newtype UserRole
  = UserRole { userRole_role :: String }
  deriving (Eq, Show, Read, Generic)

instance PG.FromField UserRole where
   fromField f mdata =
     case B.unpack `fmap` mdata of
       Nothing       -> PG.returnError PG.UnexpectedNull f ""
       Just role     -> return UserRole { userRole_role = role }

instance Aeson.ToJSON UserRole where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userRole_" :: String) }

instance Aeson.FromJSON UserRole where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("userRole_" :: String) }

updateRoleHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Maybe Bool
  -> Int
  -> UserRole
  -> m UserRole
updateRoleHandler mIsAdmin userId updateUserTestRole =
  case mIsAdmin of
    Just True -> do
      connection <- getDBConnection
      IO.liftIO $ updateUserTestRoleSql userId updateUserTestRole connection

    _ ->
      Servant.throwError Servant.err404


updateUserTestRoleSql :: Int -> UserRole -> PG.Connection -> IO UserRole
updateUserTestRoleSql userId UserRole{..} connection = do
  [ PG.Only role ] <- PG.query connection rawQuery ( userRole_role
                                                   , userId
                                                   )
  return role
  where
    rawQuery =
      [sql|
          UPDATE user_test
          SET role = ?
          WHERE id = ?
          RETURNING role
          |]


-- ** list users


listUserHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => m [UserTest]
listUserHandler = do
  connection <- getDBConnection
  IO.liftIO $ listUserTestSql connection

listUserTestSql :: PG.Connection -> IO [UserTest]
listUserTestSql connection = do
  PG.query_ connection rawQuery
  where
    rawQuery =
      [sql|
          SELECT id, firstname, lastname
          FROM user_test
          |]


-- ** error


deleteNoContentHandler :: (Reader.MonadReader Env m) => m NoContent
deleteNoContentHandler =
  return NoContent

getStatusCodeHandler :: (Except.MonadError ServerError m) => Int -> m ()
getStatusCodeHandler = \case
  200 -> return ()
  404 -> Servant.throwError Servant.err404
  _ -> Servant.throwError Servant.err404
