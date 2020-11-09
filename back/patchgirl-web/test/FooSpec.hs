{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module FooSpec where

import qualified Data.Maybe                       as Maybe
import           Data.UUID                        (UUID)
import qualified Data.UUID.V4                     as UUID
import qualified Database.PostgreSQL.Simple       as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Network.HTTP.Types               as HTTP
import           Servant
import qualified Servant.Auth.Client              as Auth
import qualified Servant.Auth.Server              as Auth
import           Servant.Client                   (ClientM, client)
import           Test.Hspec

import qualified Control.Monad                    as Monad
import qualified Control.Monad.IO.Class           as IO
import qualified Data.ByteString.UTF8             as BSU
import qualified Data.Char                        as Char
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.Int                         (Int64)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Maybe                       as Maybe
import           Data.String
import           Data.Typeable                    (Typeable)
import qualified Data.Typeable                    as Type
import qualified Data.UUID                        as UUID
import           GHC.Generics                     (Generic)
import           Hedgehog                         ((===))
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Environment.Model
import           PatchGirl.Web.Id
import           PatchGirl.Web.Server


data TAccount = TAccount
    { taccount_id       :: UUID
    , taccount_githubId :: Int
    , taccount_email    :: String
    }
    deriving (Generic, Typeable, InsertSQL, PG.ToRow, Show)


uuidGen :: Gen UUID
uuidGen = UUID.fromWords <$> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded

accountGen :: Gen TAccount
accountGen = do
  taccount_id <- uuidGen
  taccount_githubId <- Gen.int (Range.linear 1 100)
  taccount_email <- do
    emailPrefix <- Gen.string (Range.linear 1 10) Gen.alphaNum
    emailSuffix <- Gen.string (Range.linear 1 10) Gen.alphaNum
    emailDomain <- Gen.string (Range.linear 1 10) Gen.alphaNum
    return $ emailPrefix <> "@" <> emailSuffix <> "." <> emailDomain
  return TAccount{..}

insert :: (InsertSQL a, PG.ToRow a) => a -> PG.Connection -> IO Int64
insert a connection =
  PG.execute connection (asSql a) a

class (PG.ToRow a, Typeable a) => InsertSQL a where
  asSql :: a -> PG.Query
  asSql self =
    PG.Query { PG.fromQuery = BSU.fromString insertSqlQuery }
    where
      insertSqlQuery :: String
      insertSqlQuery =
        "INSERT INTO " <> tableName <> " values "

      tableName :: String
      tableName =
        Type.typeOf self & show <&> Char.toLower

spec :: Spec
spec =

  fdescribe "Yeah!" $ do
    it "Yo!" $ hedgehog $ do
--      pure () :: PropertyT IO ()
      account <- forAll accountGen
      True === True
