{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Account.DB where

import           Control.Lens                     (makeLenses)
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (listToMaybe)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Model


-- * new fake account


defaultNewFakeAccount1 :: Int
defaultNewFakeAccount1 =
  1

defaultNewFakeAccount2 :: Int
defaultNewFakeAccount2 =
  2

insertFakeAccount :: Int -> Connection -> IO UUID
insertFakeAccount githubId connection = do
  [Only id] <- query connection rawQuery (Only githubId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO account (github_id)
          VALUES (?)
          RETURNING id
          |]


-- * fake account without password


newtype NewFakeAccountWithoutPassword =
  NewFakeAccountWithoutPassword { _newFakeAccountWithoutPasswordEmail :: CaseInsensitive }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeAccountWithoutPassword :: NewFakeAccountWithoutPassword -> Connection -> IO (UUID, String)
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
  FakeAccount { _fakeAccountId          :: UUID
              , _fakeAccountEmail       :: CaseInsensitive
              , _fakeAccountPassword    :: Maybe String
              , _fakeAccountSignupToken :: String
              }
  deriving (Eq, Show, Read, Generic, FromRow)

$(makeLenses ''FakeAccount)

selectFakeAccount :: UUID -> Connection -> IO (Maybe FakeAccount)
selectFakeAccount id connection =
  query connection rawQuery (Only id) <&> listToMaybe
  where
    rawQuery =
      [sql|
          SELECT id, email, password, signup_token
          FROM account
          where id = ?
          |]
