{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE QuasiQuotes    #-}

module Session.DB where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToRow
import           DB
import           GHC.Generics
import           Model

data FakeAccount =
  FakeAccount { _fakeAccountEmail    :: CaseInsensitive
              , _fakeAccountPassword :: String
              }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeAccount :: FakeAccount -> Connection -> IO Int
insertFakeAccount fakeAccount connection = do
  [Only id] <- query connection rawQuery fakeAccount
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO account (email, password)
          VALUES (?, crypt(?, gen_salt('bf', 8)))
          RETURNING id
          |]
