module PgSqlComputation.App where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BSU
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map
import qualified Data.Maybe                as Maybe
import qualified Database.PostgreSQL.LibPQ as LibPQ

import           Env
import           Interpolator
import           PgSqlComputation.Model


-- * handler


runPgSqlComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => (EnvironmentVars, PgComputationInput)
  -> m PgComputationOutput
runPgSqlComputationHandler (environmentVars, pgComputationInput) =
  runPgComputationWithScenarioContext pgComputationInput environmentVars Map.empty Map.empty


-- * run pg computation with scenario context


runPgComputationWithScenarioContext
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => PgComputationInput
  -> EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> m PgComputationOutput
runPgComputationWithScenarioContext PgComputationInput{..} environmentVars scenarioGlobalVars scenarioLocalVars = do
  let
    sql = substitute _pgComputationInputSql
    pgConnection = PgConnection { _pgConnectionHost     = substitute (_templatedPgConnectionHost _pgComputationInputPgConnection)
                                , _pgConnectionPort     = substitute (_templatedPgConnectionPort _pgComputationInputPgConnection)
                                , _pgConnectionUser     = substitute (_templatedPgConnectionUser _pgComputationInputPgConnection)
                                , _pgConnectionPassword = substitute (_templatedPgConnectionPassword _pgComputationInputPgConnection)
                                , _pgConnectionDbName   = substitute (_templatedPgConnectionDbName _pgComputationInputPgConnection)
                                }
  (mResult, resultStatus) <- IO.liftIO $ do
    connection <- getConnection pgConnection
    mResult <- LibPQ.exec connection (BSU.fromString sql)
    case mResult of
      Nothing ->
        return (mResult, LibPQ.FatalError)
      Just result ->
        LibPQ.resultStatus result <&> \resultStatus -> (mResult, resultStatus)

  case (mResult, resultStatus) of
    (Nothing, _) ->
      IO.liftIO $ return $ Left $ PgError "fatal error"

    (Just _, LibPQ.CommandOk) ->
      IO.liftIO $ return $ Right PgCommandOK

    (Just result, LibPQ.TuplesOk) ->
      IO.liftIO $ resultToTable result <&> Right . PgTuplesOk

    (Just result, error) ->
      IO.liftIO $
        LibPQ.resultErrorField result LibPQ.DiagMessagePrimary
          <&> fmap BSU.toString
          <&> Maybe.fromMaybe ""
          <&> \primaryMessage -> (Left $ PgError (show error ++ " " ++ primaryMessage))
  where
    substitute :: StringTemplate -> String
    substitute =
      interpolate environmentVars scenarioGlobalVars scenarioLocalVars



-- * to table


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

    buildRow :: LibPQ.Oid -> LibPQ.Column -> LibPQ.Row -> IO PgValue
    buildRow oid columnIndex rowIndex = do
      mValue <- LibPQ.getvalue result rowIndex columnIndex
      case mValue of
        Nothing -> return PgNull
        Just bs -> return $ BSU.toString bs & convertPgRawValueToPgValue oid

    columnInfo :: LibPQ.Result -> LibPQ.Column -> IO (Maybe String, LibPQ.Oid)
    columnInfo result columnIndex = do
      mName <- LibPQ.fname result columnIndex <&> fmap BSU.toString
      oid <- LibPQ.ftype result columnIndex
      return (mName, oid)

-- https://github.com/rwinlib/libpq/blob/0b054b90cf6ec76f48accd2299bb90395dac7e29/include/postgresql/server/catalog/pg_type_d.h
convertPgRawValueToPgValue :: LibPQ.Oid -> String -> PgValue
convertPgRawValueToPgValue oid value =
  case oid of
    LibPQ.Oid 16 ->
      case value of
        "t" -> PgBool True
        "f" -> PgBool False
        _   -> undefined

    LibPQ.Oid 23 ->
      PgInt $ read @Int value

    LibPQ.Oid 25 ->
      PgString value

    LibPQ.Oid 2249 -> -- record
      PgString value

    LibPQ.Oid _ ->
      PgString value


-- * util


getConnection :: PgConnection -> IO LibPQ.Connection
getConnection PgConnection{..} =
  LibPQ.connectdb (BS.intercalate " " components)
  where
    components =
      [ "host=" <> BSU.fromString _pgConnectionHost
      , "port=" <> BSU.fromString _pgConnectionPort
      , "user=" <> BSU.fromString _pgConnectionUser
      , "dbname=" <> BSU.fromString _pgConnectionDbName
      {- password needs to be the last because it can be empty
         and an empty field will mess up the connection
      -}
      , "password=" <> BSU.fromString _pgConnectionPassword
      ]
