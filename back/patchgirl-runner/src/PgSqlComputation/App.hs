module PgSqlComputation.App where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BSU
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import qualified Data.Maybe                as Maybe
import qualified Database.PostgreSQL.LibPQ as LibPQ
import           Debug.Trace

import           Env
import           PgSqlComputation.Model


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
      (mColumName, oid) <- columnInfo result columnIndex
      let columnName = Maybe.fromMaybe "" mColumName
      rows <- Monad.forM [0..rowSize] (buildRow oid columnIndex)
      return $ Column columnName rows

    buildRow :: LibPQ.Oid -> LibPQ.Column -> LibPQ.Row -> IO PGValue
    buildRow oid columnIndex rowIndex = do
      mValue <- LibPQ.getvalue result rowIndex columnIndex
      case mValue of
        Nothing -> return PGNull
        Just bs -> return $ BSU.toString bs & convertBytestringToPGValue oid

convertBytestringToPGValue :: LibPQ.Oid -> String -> PGValue
convertBytestringToPGValue oid value =
  case oid of
    LibPQ.Oid 16 ->
      case value of
        "t" -> PGBool True
        "f" -> PGBool False
        _   -> undefined

    LibPQ.Oid 23 ->
      PGInt $ read @Int value

    LibPQ.Oid 25 ->
      PGString value

    _            -> undefined

columnInfo :: LibPQ.Result -> LibPQ.Column -> IO (Maybe String, LibPQ.Oid)
columnInfo result columnIndex = do
  mName <- LibPQ.fname result columnIndex <&> (fmap BSU.toString)
  oid <- LibPQ.ftype result columnIndex
  return (mName, oid)
