{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module RequestCollection.DB where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics

-- * new fake request collection


data FakeRequestCollection =
  FakeRequestCollection { _fakeRequestCollectionId        :: Int
                        , _fakeRequestCollectionAccountId :: Int
                        }
  deriving (Eq, Show, Read, Generic, FromRow)

insertFakeRequestCollection :: Int -> Connection -> IO FakeRequestCollection
insertFakeRequestCollection accountId connection = do
  [fakeRequestCollection] <- query connection rawQuery (Only accountId)
  return fakeRequestCollection
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection (account_id)
          VALUES (?)
          RETURNING id, account_id
          |]
