{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Account.DB where

import           Control.Lens                     (makeFieldsNoPrefix)
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (listToMaybe)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToRow
import           DB
import           GHC.Generics
import           Model


-- * new fake account


data NewFakeAccount =
  NewFakeAccount { _newFakeAccountEmail    :: CaseInsensitive
                 , _newFakeAccountPassword :: String
                 }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeAccount :: NewFakeAccount -> Connection -> IO (Int, String)
insertFakeAccount newFakeAccount connection = do
  [(id, signupToken)] <- query connection rawQuery newFakeAccount
  return (id, signupToken)
  where
    rawQuery =
      [sql|
          INSERT INTO account (email, password)
          VALUES (?, crypt(?, gen_salt('bf', 8)))
          RETURNING id, signup_token
          |]


-- * fake account without password


newtype NewFakeAccountWithoutPassword =
  NewFakeAccountWithoutPassword { _newFakeAccountWithoutPasswordEmail :: CaseInsensitive }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeAccountWithoutPassword :: NewFakeAccountWithoutPassword -> Connection -> IO (Int, String)
insertFakeAccountWithoutPassword newFakeAccount connection = do
  [(id, signupToken)] <- query connection rawQuery newFakeAccount
  return (id, signupToken)
  where
    rawQuery =
      [sql|
          INSERT INTO account (email) values (?)
          RETURNING id, signup_token
          |]

-- * fake account


data FakeAccount =
  FakeAccount { _fakeAccountId          :: Int
              , _fakeAccountEmail       :: CaseInsensitive
              , _fakeAccountPassword    :: Maybe String
              , _fakeAccountSignupToken :: String
              }
  deriving (Eq, Show, Read, Generic, FromRow)

$(makeFieldsNoPrefix ''FakeAccount)

selectFakeAccount :: Int -> Connection -> IO (Maybe FakeAccount)
selectFakeAccount id connection =
  query connection rawQuery (Only id) <&> listToMaybe
  where
    rawQuery =
      [sql|
          SELECT id, email, password, signup_token
          FROM account
          where id = ?
          |]
