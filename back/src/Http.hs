{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Http where

import           Data.Aeson
import qualified Data.ByteString.Char8                as B
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import           GHC.Generics

-- * method


data Method
  = Get
  | Post
  | Put
  | Delete
  | Patch
  | Head
  | Options
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance PG.FromField Method where
   fromField f mdata =
     case B.unpack `fmap` mdata of
       Nothing        -> PG.returnError PG.UnexpectedNull f ""
       Just "Get"     -> return Get
       Just "Post"    -> return Post
       Just "Put"     -> return Put
       Just "Delete"  -> return Delete
       Just "Patch"   -> return Patch
       Just "Head"    -> return Head
       Just "Options" -> return Options
       _              -> PG.returnError PG.Incompatible f ""

methodToString :: Method -> String
methodToString = \case
  Get -> "GET"
  Post -> "POST"
  Put -> "PUT"
  Delete -> "DELETE"
  Patch -> "PATCH"
  Head -> "HEAD"
  Options -> "OPTIONS"

instance PG.ToField Method where
  toField = PG.toField . show


-- * scheme


data Scheme
  = Http
  | Https
  deriving (Eq, Show, Read, Generic)

instance ToJSON Scheme
instance FromJSON Scheme

schemeToString :: Scheme -> String
schemeToString = \case
  Http -> "http"
  Https -> "https"
