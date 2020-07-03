{-# LANGUAGE DeriveGeneric #-}

module PgSqlComputation.App where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BSU
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import qualified Data.Maybe                as Maybe
import qualified Database.PostgreSQL.LibPQ as LibPQ
import           Debug.Trace
import           GHC.Generics              (Generic)

import           Env


-- * handler


runPgSqlComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => String
  -> m (Maybe Table)
runPgSqlComputationHandler rawSql = do
  connection <- IO.liftIO $ getConnection
  IO.liftIO $ getResult connection rawSql >>= Monad.mapM resultToTable

getConnection :: IO LibPQ.Connection
getConnection =
  LibPQ.connectdb (BS.intercalate " " components)
  where
    components =
      [ "host=" <> "localhost"
      , "port=" <> "5433"
      , "user=" <> "dev"
      , "password=" <> ""
      , "dbname=" <> "dev"
      ]

getResult :: LibPQ.Connection -> String -> IO (Maybe LibPQ.Result)
getResult connection rawSql =
  LibPQ.exec connection (BSU.fromString rawSql)

resultToTable :: LibPQ.Result -> IO Table
resultToTable result = do
  columnSize <- LibPQ.nfields result <&> \c -> c - 1
  rowSize <- LibPQ.ntuples result <&> \r -> r - 1
  columns <- Monad.forM [0..columnSize] (buildColumn rowSize)
  return $ Table columns
  where
    buildColumn :: LibPQ.Row -> LibPQ.Column -> IO Column
    buildColumn rowSize columnIndex = do
      columName <- columnInfo result columnIndex <&> Maybe.fromMaybe ""
      rows <- Monad.forM [0..rowSize] (buildRow columnIndex)
      return $ Column columName rows

    buildRow :: LibPQ.Column -> LibPQ.Row -> IO String
    buildRow columnIndex rowIndex = do
      LibPQ.getvalue result rowIndex columnIndex <&> \mBS ->
        Maybe.fromMaybe "NULL" mBS &
        BSU.toString


columnInfo :: LibPQ.Result -> LibPQ.Column -> IO (Maybe String)
columnInfo result columnIndex =
  LibPQ.fname result columnIndex <&> (fmap BSU.toString)


-- * model


data Table = Table [Column] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Table where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Table where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

data Column = Column String [String] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Column where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Column where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
