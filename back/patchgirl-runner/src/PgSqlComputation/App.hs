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
import qualified Data.Traversable          as Traversable
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Text.Read                 as Text

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
      IO.liftIO $ return $ Left $ PgError "FatalError: check the pg database connection"

    (Just _, LibPQ.CommandOk) ->
      IO.liftIO $ return $ Right PgCommandOK

    (Just result, LibPQ.TuplesOk) ->
      IO.liftIO $ resultToTable result <&> \case
        Left pgError -> Left pgError
        Right pgTable -> Right $ PgTuplesOk pgTable

    (Just result, error) ->
      IO.liftIO $
        LibPQ.resultErrorField result LibPQ.DiagMessagePrimary
          <&> fmap BSU.toString
          <&> Maybe.fromMaybe ""
          <&> \primaryMessage -> Left $ PgError (show error ++ " " ++ primaryMessage)
  where
    substitute :: StringTemplate -> String
    substitute =
      interpolate environmentVars scenarioGlobalVars scenarioLocalVars



-- * to table


resultToTable :: LibPQ.Result -> IO (Either PgError Table)
resultToTable result = do
  columnSize <- LibPQ.nfields result <&> \c -> c - 1
  rowSize <- LibPQ.ntuples result <&> \r -> r - 1
  eColumns :: Either PgError [Column] <- Monad.forM [0..columnSize] (buildColumn rowSize) <&> Traversable.sequence
  return $ eColumns <&> Table
  where
    buildColumn :: LibPQ.Row -> LibPQ.Column -> IO (Either PgError Column)
    buildColumn rowSize columnIndex = do
      (mColumName, oid) <- columnInfo result columnIndex
      let columnName = Maybe.fromMaybe "" mColumName
      rows :: Either PgError [PgValue] <- Traversable.forM [0..rowSize] (buildRow oid columnIndex) <&> Traversable.sequence
      case rows of
        Left error     -> return $ Left error
        Right pgValues -> return $ Right $ Column columnName pgValues

    buildRow :: LibPQ.Oid -> LibPQ.Column -> LibPQ.Row -> IO (Either PgError PgValue)
    buildRow oid columnIndex rowIndex = do
      mValue <- LibPQ.getvalue result rowIndex columnIndex
      case mValue of
        Nothing -> return $ Right PgNull
        Just bs -> return $ BSU.toString bs & convertPgRawValueToPgValue oid

    columnInfo :: LibPQ.Result -> LibPQ.Column -> IO (Maybe String, LibPQ.Oid)
    columnInfo result columnIndex = do
      mName <- LibPQ.fname result columnIndex <&> fmap BSU.toString
      oid <- LibPQ.ftype result columnIndex
      return (mName, oid)

-- https://github.com/rwinlib/libpq/blob/0b054b90cf6ec76f48accd2299bb90395dac7e29/include/postgresql/server/catalog/pg_type_d.h
-- select pg_typeof('whatever'::text) to see the real value type
convertPgRawValueToPgValue :: LibPQ.Oid -> String -> Either PgError PgValue
convertPgRawValueToPgValue oid value =
  let
    conversion :: Either String PgValue
    conversion = case oid of
      LibPQ.Oid 16 ->
        Right $ case value of
          "t" -> PgBool True
          "f" -> PgBool False
          _   -> PgString value

      LibPQ.Oid 20 -> -- smallint
        Text.readEither @Int value <&> PgInt

      LibPQ.Oid 21 -> -- integer
        Text.readEither @Int value <&> PgInt

      LibPQ.Oid 23 -> -- bigint
        Text.readEither @Int value <&> PgInt

      LibPQ.Oid 700 -> -- real
        Text.readEither @Float value <&> PgFloat

      LibPQ.Oid 1700 -> -- numeric
        Text.readEither @Float value <&> PgFloat

      LibPQ.Oid 701 -> -- double precision
        Text.readEither @Float value <&> PgFloat

      LibPQ.Oid 25 -> -- text
        Right $ PgString value

      LibPQ.Oid 1043 -> -- varchar
        Right $ PgString value

      LibPQ.Oid 2249 -> -- record
        Right $ PgString value

      LibPQ.Oid _ ->
        Right $ PgString value
  in
    case conversion of
      Right x  -> Right x
      Left str -> Left $ PgError str


-- * pg runner


ioPgRunner :: LibPQ.Connection -> BSU.ByteString -> IO (Maybe LibPQ.Result)
ioPgRunner =
  LibPQ.exec


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
