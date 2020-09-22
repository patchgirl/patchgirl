{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

module ScenarioComputation.Model where


import           Control.Lens             (makeLenses)
import qualified Data.Aeson               as Aeson
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)

import           Interpolator
import           PgSqlComputation.Model
import           RequestComputation.Model
import           TangoScript.Model


-- * scenario input


data ScenarioInput
  = ScenarioInput { _scenarioInputId      :: UUID
                  , _scenarioInputScenes  :: [SceneFile]
                  , _scenarioInputEnvVars :: EnvironmentVars
                  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene


data SceneFile
  = HttpSceneFile { _sceneId         :: UUID
                  , _sceneFileId     :: UUID
                  , _scenePrescript  :: TangoAst
                  , _scenePostscript :: TangoAst
                  , _sceneHttpInput  :: TemplatedRequestComputationInput
                  }
  | PgSceneFile { _sceneId         :: UUID
                , _sceneFileId     :: UUID
                , _scenePrescript  :: TangoAst
                , _scenePostscript :: TangoAst
                , _scenePgInput    :: PgComputationInput
                }
  deriving (Eq, Show, Generic)

$(makeLenses ''SceneFile)

instance Aeson.ToJSON SceneFile where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SceneFile where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario output


newtype ScenarioOutput
  = ScenarioOutput [SceneOutput]
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene output


data SceneOutput
  = SceneOutput { _outputSceneId                :: UUID
                , _outputSceneRequestFileNodeId :: UUID
                , _outputSceneComputation       :: SceneComputation
                }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SceneOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene computation output


data SceneComputation
  = SceneNotRun
  | PrescriptFailed ScriptException
  | HttpSceneOk RequestComputation
  | HttpSceneFailed HttpException
  | PgSceneOk PgComputation
  | PgSceneFailed PgError
  | PgPostscriptFailed PgComputation ScriptException
  | HttpPostscriptFailed RequestComputation ScriptException
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                             , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                             }

instance Aeson.FromJSON SceneComputation where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                                , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                                }


-- * script exception


data ScriptException
  = UnknownVariable Expr
  | AssertEqualFailed Expr Expr
  | CannotUseFunction String
  | EmptyResponse String
  | AccessOutOfBound Expr Expr
  | CantAccessElem Expr Expr
  | ConversionFailed Expr String
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScriptException where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScriptException where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * context


data Context a where
  PreScene :: Context a
  PostScene :: ToPrimitive a => a -> Context a


-- * primitive


class ToPrimitive a where
  getBody :: a -> Either ScriptException String
  getStatus :: a -> Either ScriptException Expr
  getSimpleTable :: a -> Either ScriptException Expr
  getRichTable :: a -> Either ScriptException Expr


-- ** request computation


instance ToPrimitive RequestComputation where
  getBody RequestComputation {..} =
    Right _requestComputationBody

  getStatus RequestComputation {..} =
    Right $ LInt _requestComputationStatusCode

  getSimpleTable _ =
    Left $ CannotUseFunction "function not available for http request"

  getRichTable _ =
    Left $ CannotUseFunction "function not available for http request"


-- ** pg computation


instance ToPrimitive PgComputation where
  getBody _ =
    Left $ CannotUseFunction "not available for pg query"

  getStatus _ =
    Left $ CannotUseFunction "not available for pg query"

  getSimpleTable = \case
    PgCommandOK ->
      Left $ EmptyResponse "empty response"

    PgTuplesOk table ->
      Right $ fromTableToSimpleList table

  getRichTable = \case
    PgCommandOK ->
      Left $ EmptyResponse "empty response"

    PgTuplesOk table ->
      Right $ fromTableToRichList table


-- * expr


fromTableToSimpleList :: [Row] -> Expr
fromTableToSimpleList rows =
  LList $ map rowToExpr rows
  where
    rowToExpr :: Row -> Expr
    rowToExpr (Row row) =
      LList $ map pgValueToExpr row

    pgValueToExpr :: (String, PgValue) -> Expr
    pgValueToExpr (_, pgValue) = case pgValue of
      PgString x -> LString x
      PgInt x    -> LInt x
      PgFloat x  -> LFloat x
      PgBool x   -> LBool x
      PgNull     -> LNull

fromTableToRichList :: [Row] -> Expr
fromTableToRichList rows =
  LList $ map rowToExpr rows
  where
    rowToExpr :: Row -> Expr
    rowToExpr (Row row) =
      LList $ map pgValueToExpr row

    pgValueToExpr :: (String, PgValue) -> Expr
    pgValueToExpr (name, pgValue) = case pgValue of
      PgString x -> LRowElem(name, LString x)
      PgInt x    -> LRowElem(name, LInt x)
      PgFloat x  -> LRowElem(name, LFloat x)
      PgBool x   -> LRowElem(name, LBool x)
      PgNull     -> LRowElem(name, LNull)


-- * script context


data ScriptContext =
  ScriptContext { environmentVars :: EnvironmentVars
                , globalVars      :: ScenarioVars
                , localVars       :: ScenarioVars
                }


buildSceneOutput :: SceneFile -> SceneComputation -> SceneOutput
buildSceneOutput scene sceneComputationOutput =
  SceneOutput { _outputSceneId = _sceneId scene
              , _outputSceneRequestFileNodeId = _sceneFileId scene
              , _outputSceneComputation = sceneComputationOutput
              }
