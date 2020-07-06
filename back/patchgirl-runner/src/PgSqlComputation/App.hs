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
  -> m PGComputation
runPgSqlComputationHandler rawSql = do
  connection <- IO.liftIO $ getConnection
  mResult <- IO.liftIO $ LibPQ.exec connection (BSU.fromString rawSql)
  resultStatus <- IO.liftIO $
    case mResult of
      Nothing ->
        return LibPQ.FatalError
      Just result ->
        LibPQ.resultStatus result

  IO.liftIO $ case (mResult, resultStatus) of
    (Nothing, _) ->
      return $ PGError "fatal error"

    (Just _, LibPQ.CommandOk) ->
      return $ PGCommandOK

    (Just result, LibPQ.TuplesOk) ->
      resultToTable result <&> PGTuplesOk

    (_, error) ->
      return $ PGError (show error)

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
        Just bs -> return $ BSU.toString bs & convertPGRawValueToPGValue oid

    columnInfo :: LibPQ.Result -> LibPQ.Column -> IO (Maybe String, LibPQ.Oid)
    columnInfo result columnIndex = do
      mName <- LibPQ.fname result columnIndex <&> (fmap BSU.toString)
      oid <- LibPQ.ftype result columnIndex
      return (mName, oid)

-- https://github.com/rwinlib/libpq/blob/0b054b90cf6ec76f48accd2299bb90395dac7e29/include/postgresql/server/catalog/pg_type_d.h
convertPGRawValueToPGValue :: LibPQ.Oid -> String -> PGValue
convertPGRawValueToPGValue oid value =
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

    LibPQ.Oid 2249 -> -- record
      PGString value

    LibPQ.Oid oid ->
      traceShow ("oid" ++ show oid) undefined


-- * util


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
