module Api.RunnerGeneratedClient exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder
import Uuid as Uuid
import Api.WebGeneratedClient exposing(..)

type alias UUID = Uuid.Uuid

jsonDecUUID : Json.Decode.Decoder UUID
jsonDecUUID = Uuid.decoder

jsonEncUUID : UUID -> Value
jsonEncUUID = Uuid.encode

type alias Either a b = Result a b

jsonDecEither : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Either a b)
jsonDecEither decoder1 decoder2 =
  Json.Decode.oneOf [ Json.Decode.field "Left" decoder1 |> Json.Decode.map Err
                    , Json.Decode.field "Right" decoder2 |> Json.Decode.map Ok
                    ]

jsonEncEither : (a -> Value) -> (b -> Value) -> Either a b -> Value
jsonEncEither encoder1 encoder2 either =
  case either of
    Err err -> encoder1 err
    Ok ok -> encoder2 ok


type alias RequestComputationInput  =
   { requestComputationInputMethod: Method
   , requestComputationInputHeaders: (List (String, String))
   , requestComputationInputUrl: String
   , requestComputationInputBody: String
   }

jsonDecRequestComputationInput : Json.Decode.Decoder ( RequestComputationInput )
jsonDecRequestComputationInput =
   Json.Decode.succeed (\prequestComputationInputMethod prequestComputationInputHeaders prequestComputationInputUrl prequestComputationInputBody -> {requestComputationInputMethod = prequestComputationInputMethod, requestComputationInputHeaders = prequestComputationInputHeaders, requestComputationInputUrl = prequestComputationInputUrl, requestComputationInputBody = prequestComputationInputBody})
   |> required "requestComputationInputMethod" (jsonDecMethod)
   |> required "requestComputationInputHeaders" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))
   |> required "requestComputationInputUrl" (Json.Decode.string)
   |> required "requestComputationInputBody" (Json.Decode.string)

jsonEncRequestComputationInput : RequestComputationInput -> Value
jsonEncRequestComputationInput  val =
   Json.Encode.object
   [ ("requestComputationInputMethod", jsonEncMethod val.requestComputationInputMethod)
   , ("requestComputationInputHeaders", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.string) t2])) val.requestComputationInputHeaders)
   , ("requestComputationInputUrl", Json.Encode.string val.requestComputationInputUrl)
   , ("requestComputationInputBody", Json.Encode.string val.requestComputationInputBody)
   ]



type alias RequestComputationOutput  =
   { requestComputationOutputStatusCode: Int
   , requestComputationOutputHeaders: (List (String, String))
   , requestComputationOutputBody: String
   }

jsonDecRequestComputationOutput : Json.Decode.Decoder ( RequestComputationOutput )
jsonDecRequestComputationOutput =
   Json.Decode.succeed (\prequestComputationOutputStatusCode prequestComputationOutputHeaders prequestComputationOutputBody -> {requestComputationOutputStatusCode = prequestComputationOutputStatusCode, requestComputationOutputHeaders = prequestComputationOutputHeaders, requestComputationOutputBody = prequestComputationOutputBody})
   |> required "requestComputationOutputStatusCode" (Json.Decode.int)
   |> required "requestComputationOutputHeaders" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))
   |> required "requestComputationOutputBody" (Json.Decode.string)

jsonEncRequestComputationOutput : RequestComputationOutput -> Value
jsonEncRequestComputationOutput  val =
   Json.Encode.object
   [ ("requestComputationOutputStatusCode", Json.Encode.int val.requestComputationOutputStatusCode)
   , ("requestComputationOutputHeaders", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.string) t2])) val.requestComputationOutputHeaders)
   , ("requestComputationOutputBody", Json.Encode.string val.requestComputationOutputBody)
   ]



type alias RequestComputationResult  = (Either HttpException RequestComputationOutput)

jsonDecRequestComputationResult : Json.Decode.Decoder ( RequestComputationResult )
jsonDecRequestComputationResult =
    jsonDecEither (jsonDecHttpException) (jsonDecRequestComputationOutput)

jsonEncRequestComputationResult : RequestComputationResult -> Value
jsonEncRequestComputationResult  val = (jsonEncEither (jsonEncHttpException) (jsonEncRequestComputationOutput)) val



type ScriptException  =
    UnknownVariable Expr
    | AssertEqualFailed Expr Expr

jsonDecScriptException : Json.Decode.Decoder ( ScriptException )
jsonDecScriptException =
    let jsonDecDictScriptException = Dict.fromList
            [ ("UnknownVariable", Json.Decode.lazy (\_ -> Json.Decode.map UnknownVariable (jsonDecExpr)))
            , ("AssertEqualFailed", Json.Decode.lazy (\_ -> Json.Decode.map2 AssertEqualFailed (Json.Decode.index 0 (jsonDecExpr)) (Json.Decode.index 1 (jsonDecExpr))))
            ]
        jsonDecObjectSetScriptException = Set.fromList []
    in  decodeSumTaggedObject "ScriptException" "tag" "contents" jsonDecDictScriptException jsonDecObjectSetScriptException

jsonEncScriptException : ScriptException -> Value
jsonEncScriptException  val =
    let keyval v = case v of
                    UnknownVariable v1 -> ("UnknownVariable", encodeValue (jsonEncExpr v1))
                    AssertEqualFailed v1 v2 -> ("AssertEqualFailed", encodeValue (Json.Encode.list identity [jsonEncExpr v1, jsonEncExpr v2]))
    in encodeSumTaggedObject "tag" "contents" keyval val



type HttpException  =
    InvalidUrlException String String
    | TooManyRedirects 
    | OverlongHeaders 
    | ResponseTimeout 
    | ConnectionTimeout 
    | ConnectionFailure String
    | InvalidStatusLine 
    | InvalidHeader 
    | InvalidRequestHeader 
    | InternalException String
    | ProxyConnectException 
    | NoResponseDataReceived 
    | WrongRequestBodyStreamSize 
    | ResponseBodyTooShort 
    | InvalidChunkHeaders 
    | IncompleteHeaders 
    | InvalidDestinationHost 
    | HttpZlibException 
    | InvalidProxyEnvironmentVariable 
    | ConnectionClosed 
    | InvalidProxySettings 
    | UnknownException 

jsonDecHttpException : Json.Decode.Decoder ( HttpException )
jsonDecHttpException =
    let jsonDecDictHttpException = Dict.fromList
            [ ("InvalidUrlException", Json.Decode.lazy (\_ -> Json.Decode.map2 InvalidUrlException (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))
            , ("TooManyRedirects", Json.Decode.lazy (\_ -> Json.Decode.succeed TooManyRedirects))
            , ("OverlongHeaders", Json.Decode.lazy (\_ -> Json.Decode.succeed OverlongHeaders))
            , ("ResponseTimeout", Json.Decode.lazy (\_ -> Json.Decode.succeed ResponseTimeout))
            , ("ConnectionTimeout", Json.Decode.lazy (\_ -> Json.Decode.succeed ConnectionTimeout))
            , ("ConnectionFailure", Json.Decode.lazy (\_ -> Json.Decode.map ConnectionFailure (Json.Decode.string)))
            , ("InvalidStatusLine", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidStatusLine))
            , ("InvalidHeader", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidHeader))
            , ("InvalidRequestHeader", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidRequestHeader))
            , ("InternalException", Json.Decode.lazy (\_ -> Json.Decode.map InternalException (Json.Decode.string)))
            , ("ProxyConnectException", Json.Decode.lazy (\_ -> Json.Decode.succeed ProxyConnectException))
            , ("NoResponseDataReceived", Json.Decode.lazy (\_ -> Json.Decode.succeed NoResponseDataReceived))
            , ("WrongRequestBodyStreamSize", Json.Decode.lazy (\_ -> Json.Decode.succeed WrongRequestBodyStreamSize))
            , ("ResponseBodyTooShort", Json.Decode.lazy (\_ -> Json.Decode.succeed ResponseBodyTooShort))
            , ("InvalidChunkHeaders", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidChunkHeaders))
            , ("IncompleteHeaders", Json.Decode.lazy (\_ -> Json.Decode.succeed IncompleteHeaders))
            , ("InvalidDestinationHost", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidDestinationHost))
            , ("HttpZlibException", Json.Decode.lazy (\_ -> Json.Decode.succeed HttpZlibException))
            , ("InvalidProxyEnvironmentVariable", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidProxyEnvironmentVariable))
            , ("ConnectionClosed", Json.Decode.lazy (\_ -> Json.Decode.succeed ConnectionClosed))
            , ("InvalidProxySettings", Json.Decode.lazy (\_ -> Json.Decode.succeed InvalidProxySettings))
            , ("UnknownException", Json.Decode.lazy (\_ -> Json.Decode.succeed UnknownException))
            ]
    in  decodeSumObjectWithSingleField  "HttpException" jsonDecDictHttpException

jsonEncHttpException : HttpException -> Value
jsonEncHttpException  val =
    let keyval v = case v of
                    InvalidUrlException v1 v2 -> ("InvalidUrlException", encodeValue (Json.Encode.list identity [Json.Encode.string v1, Json.Encode.string v2]))
                    TooManyRedirects  -> ("TooManyRedirects", encodeValue (Json.Encode.list identity []))
                    OverlongHeaders  -> ("OverlongHeaders", encodeValue (Json.Encode.list identity []))
                    ResponseTimeout  -> ("ResponseTimeout", encodeValue (Json.Encode.list identity []))
                    ConnectionTimeout  -> ("ConnectionTimeout", encodeValue (Json.Encode.list identity []))
                    ConnectionFailure v1 -> ("ConnectionFailure", encodeValue (Json.Encode.string v1))
                    InvalidStatusLine  -> ("InvalidStatusLine", encodeValue (Json.Encode.list identity []))
                    InvalidHeader  -> ("InvalidHeader", encodeValue (Json.Encode.list identity []))
                    InvalidRequestHeader  -> ("InvalidRequestHeader", encodeValue (Json.Encode.list identity []))
                    InternalException v1 -> ("InternalException", encodeValue (Json.Encode.string v1))
                    ProxyConnectException  -> ("ProxyConnectException", encodeValue (Json.Encode.list identity []))
                    NoResponseDataReceived  -> ("NoResponseDataReceived", encodeValue (Json.Encode.list identity []))
                    WrongRequestBodyStreamSize  -> ("WrongRequestBodyStreamSize", encodeValue (Json.Encode.list identity []))
                    ResponseBodyTooShort  -> ("ResponseBodyTooShort", encodeValue (Json.Encode.list identity []))
                    InvalidChunkHeaders  -> ("InvalidChunkHeaders", encodeValue (Json.Encode.list identity []))
                    IncompleteHeaders  -> ("IncompleteHeaders", encodeValue (Json.Encode.list identity []))
                    InvalidDestinationHost  -> ("InvalidDestinationHost", encodeValue (Json.Encode.list identity []))
                    HttpZlibException  -> ("HttpZlibException", encodeValue (Json.Encode.list identity []))
                    InvalidProxyEnvironmentVariable  -> ("InvalidProxyEnvironmentVariable", encodeValue (Json.Encode.list identity []))
                    ConnectionClosed  -> ("ConnectionClosed", encodeValue (Json.Encode.list identity []))
                    InvalidProxySettings  -> ("InvalidProxySettings", encodeValue (Json.Encode.list identity []))
                    UnknownException  -> ("UnknownException", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type Template  =
    Sentence String
    | Key String

jsonDecTemplate : Json.Decode.Decoder ( Template )
jsonDecTemplate =
    let jsonDecDictTemplate = Dict.fromList
            [ ("Sentence", Json.Decode.lazy (\_ -> Json.Decode.map Sentence (Json.Decode.string)))
            , ("Key", Json.Decode.lazy (\_ -> Json.Decode.map Key (Json.Decode.string)))
            ]
        jsonDecObjectSetTemplate = Set.fromList []
    in  decodeSumTaggedObject "Template" "tag" "contents" jsonDecDictTemplate jsonDecObjectSetTemplate

jsonEncTemplate : Template -> Value
jsonEncTemplate  val =
    let keyval v = case v of
                    Sentence v1 -> ("Sentence", encodeValue (Json.Encode.string v1))
                    Key v1 -> ("Key", encodeValue (Json.Encode.string v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias StringTemplate  = (List Template)

jsonDecStringTemplate : Json.Decode.Decoder ( StringTemplate )
jsonDecStringTemplate =
    Json.Decode.list (jsonDecTemplate)

jsonEncStringTemplate : StringTemplate -> Value
jsonEncStringTemplate  val = (Json.Encode.list jsonEncTemplate) val



type alias TemplatedRequestComputationInput  =
   { templatedRequestComputationInputMethod: Method
   , templatedRequestComputationInputHeaders: (List ((List Template), (List Template)))
   , templatedRequestComputationInputUrl: (List Template)
   , templatedRequestComputationInputBody: (List Template)
   }

jsonDecTemplatedRequestComputationInput : Json.Decode.Decoder ( TemplatedRequestComputationInput )
jsonDecTemplatedRequestComputationInput =
   Json.Decode.succeed (\ptemplatedRequestComputationInputMethod ptemplatedRequestComputationInputHeaders ptemplatedRequestComputationInputUrl ptemplatedRequestComputationInputBody -> {templatedRequestComputationInputMethod = ptemplatedRequestComputationInputMethod, templatedRequestComputationInputHeaders = ptemplatedRequestComputationInputHeaders, templatedRequestComputationInputUrl = ptemplatedRequestComputationInputUrl, templatedRequestComputationInputBody = ptemplatedRequestComputationInputBody})
   |> required "templatedRequestComputationInputMethod" (jsonDecMethod)
   |> required "templatedRequestComputationInputHeaders" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.list (jsonDecTemplate))) (Json.Decode.index 1 (Json.Decode.list (jsonDecTemplate)))))
   |> required "templatedRequestComputationInputUrl" (Json.Decode.list (jsonDecTemplate))
   |> required "templatedRequestComputationInputBody" (Json.Decode.list (jsonDecTemplate))

jsonEncTemplatedRequestComputationInput : TemplatedRequestComputationInput -> Value
jsonEncTemplatedRequestComputationInput  val =
   Json.Encode.object
   [ ("templatedRequestComputationInputMethod", jsonEncMethod val.templatedRequestComputationInputMethod)
   , ("templatedRequestComputationInputHeaders", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [((Json.Encode.list jsonEncTemplate)) t1,((Json.Encode.list jsonEncTemplate)) t2])) val.templatedRequestComputationInputHeaders)
   , ("templatedRequestComputationInputUrl", (Json.Encode.list jsonEncTemplate) val.templatedRequestComputationInputUrl)
   , ("templatedRequestComputationInputBody", (Json.Encode.list jsonEncTemplate) val.templatedRequestComputationInputBody)
   ]



type alias ScenarioOutput  = (List SceneOutput)

jsonDecScenarioOutput : Json.Decode.Decoder ( ScenarioOutput )
jsonDecScenarioOutput =
    Json.Decode.list (jsonDecSceneOutput)

jsonEncScenarioOutput : ScenarioOutput -> Value
jsonEncScenarioOutput  val = (Json.Encode.list jsonEncSceneOutput) val



type alias ScenarioInput  =
   { scenarioInputId: UUID
   , scenarioInputScenes: (List SceneInput)
   , scenarioInputEnvVars: EnvironmentVars
   }

jsonDecScenarioInput : Json.Decode.Decoder ( ScenarioInput )
jsonDecScenarioInput =
   Json.Decode.succeed (\pscenarioInputId pscenarioInputScenes pscenarioInputEnvVars -> {scenarioInputId = pscenarioInputId, scenarioInputScenes = pscenarioInputScenes, scenarioInputEnvVars = pscenarioInputEnvVars})
   |> required "scenarioInputId" (jsonDecUUID)
   |> required "scenarioInputScenes" (Json.Decode.list (jsonDecSceneInput))
   |> required "scenarioInputEnvVars" (jsonDecEnvironmentVars)

jsonEncScenarioInput : ScenarioInput -> Value
jsonEncScenarioInput  val =
   Json.Encode.object
   [ ("scenarioInputId", jsonEncUUID val.scenarioInputId)
   , ("scenarioInputScenes", (Json.Encode.list jsonEncSceneInput) val.scenarioInputScenes)
   , ("scenarioInputEnvVars", jsonEncEnvironmentVars val.scenarioInputEnvVars)
   ]



type alias SceneOutput  =
   { outputSceneId: UUID
   , outputSceneRequestFileNodeId: UUID
   , outputSceneComputation: SceneComputation
   }

jsonDecSceneOutput : Json.Decode.Decoder ( SceneOutput )
jsonDecSceneOutput =
   Json.Decode.succeed (\poutputSceneId poutputSceneRequestFileNodeId poutputSceneComputation -> {outputSceneId = poutputSceneId, outputSceneRequestFileNodeId = poutputSceneRequestFileNodeId, outputSceneComputation = poutputSceneComputation})
   |> required "outputSceneId" (jsonDecUUID)
   |> required "outputSceneRequestFileNodeId" (jsonDecUUID)
   |> required "outputSceneComputation" (jsonDecSceneComputation)

jsonEncSceneOutput : SceneOutput -> Value
jsonEncSceneOutput  val =
   Json.Encode.object
   [ ("outputSceneId", jsonEncUUID val.outputSceneId)
   , ("outputSceneRequestFileNodeId", jsonEncUUID val.outputSceneRequestFileNodeId)
   , ("outputSceneComputation", jsonEncSceneComputation val.outputSceneComputation)
   ]



type alias SceneInput  =
   { sceneInputId: UUID
   , sceneInputRequestFileNodeId: UUID
   , sceneInputPrescript: TangoAst
   , sceneInputPostscript: TangoAst
   , sceneInputTemplatedRequestComputationInput: TemplatedRequestComputationInput
   }

jsonDecSceneInput : Json.Decode.Decoder ( SceneInput )
jsonDecSceneInput =
   Json.Decode.succeed (\psceneInputId psceneInputRequestFileNodeId psceneInputPrescript psceneInputPostscript psceneInputTemplatedRequestComputationInput -> {sceneInputId = psceneInputId, sceneInputRequestFileNodeId = psceneInputRequestFileNodeId, sceneInputPrescript = psceneInputPrescript, sceneInputPostscript = psceneInputPostscript, sceneInputTemplatedRequestComputationInput = psceneInputTemplatedRequestComputationInput})
   |> required "sceneInputId" (jsonDecUUID)
   |> required "sceneInputRequestFileNodeId" (jsonDecUUID)
   |> required "sceneInputPrescript" (jsonDecTangoAst)
   |> required "sceneInputPostscript" (jsonDecTangoAst)
   |> required "sceneInputTemplatedRequestComputationInput" (jsonDecTemplatedRequestComputationInput)

jsonEncSceneInput : SceneInput -> Value
jsonEncSceneInput  val =
   Json.Encode.object
   [ ("sceneInputId", jsonEncUUID val.sceneInputId)
   , ("sceneInputRequestFileNodeId", jsonEncUUID val.sceneInputRequestFileNodeId)
   , ("sceneInputPrescript", jsonEncTangoAst val.sceneInputPrescript)
   , ("sceneInputPostscript", jsonEncTangoAst val.sceneInputPostscript)
   , ("sceneInputTemplatedRequestComputationInput", jsonEncTemplatedRequestComputationInput val.sceneInputTemplatedRequestComputationInput)
   ]



type Expr  =
    LBool Bool
    | LInt Int
    | LString String
    | Var String
    | Fetch String
    | Eq Expr Expr
    | Add Expr Expr
    | HttpResponseBodyAsString 
    | HttpResponseStatus 

jsonDecExpr : Json.Decode.Decoder ( Expr )
jsonDecExpr =
    let jsonDecDictExpr = Dict.fromList
            [ ("LBool", Json.Decode.lazy (\_ -> Json.Decode.map LBool (Json.Decode.bool)))
            , ("LInt", Json.Decode.lazy (\_ -> Json.Decode.map LInt (Json.Decode.int)))
            , ("LString", Json.Decode.lazy (\_ -> Json.Decode.map LString (Json.Decode.string)))
            , ("Var", Json.Decode.lazy (\_ -> Json.Decode.map Var (Json.Decode.string)))
            , ("Fetch", Json.Decode.lazy (\_ -> Json.Decode.map Fetch (Json.Decode.string)))
            , ("Eq", Json.Decode.lazy (\_ -> Json.Decode.map2 Eq (Json.Decode.index 0 (jsonDecExpr)) (Json.Decode.index 1 (jsonDecExpr))))
            , ("Add", Json.Decode.lazy (\_ -> Json.Decode.map2 Add (Json.Decode.index 0 (jsonDecExpr)) (Json.Decode.index 1 (jsonDecExpr))))
            , ("HttpResponseBodyAsString", Json.Decode.lazy (\_ -> Json.Decode.succeed HttpResponseBodyAsString))
            , ("HttpResponseStatus", Json.Decode.lazy (\_ -> Json.Decode.succeed HttpResponseStatus))
            ]
        jsonDecObjectSetExpr = Set.fromList []
    in  decodeSumTaggedObject "Expr" "tag" "contents" jsonDecDictExpr jsonDecObjectSetExpr

jsonEncExpr : Expr -> Value
jsonEncExpr  val =
    let keyval v = case v of
                    LBool v1 -> ("LBool", encodeValue (Json.Encode.bool v1))
                    LInt v1 -> ("LInt", encodeValue (Json.Encode.int v1))
                    LString v1 -> ("LString", encodeValue (Json.Encode.string v1))
                    Var v1 -> ("Var", encodeValue (Json.Encode.string v1))
                    Fetch v1 -> ("Fetch", encodeValue (Json.Encode.string v1))
                    Eq v1 v2 -> ("Eq", encodeValue (Json.Encode.list identity [jsonEncExpr v1, jsonEncExpr v2]))
                    Add v1 v2 -> ("Add", encodeValue (Json.Encode.list identity [jsonEncExpr v1, jsonEncExpr v2]))
                    HttpResponseBodyAsString  -> ("HttpResponseBodyAsString", encodeValue (Json.Encode.list identity []))
                    HttpResponseStatus  -> ("HttpResponseStatus", encodeValue (Json.Encode.list identity []))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias TangoAst  = (List Proc)

jsonDecTangoAst : Json.Decode.Decoder ( TangoAst )
jsonDecTangoAst =
    Json.Decode.list (jsonDecProc)

jsonEncTangoAst : TangoAst -> Value
jsonEncTangoAst  val = (Json.Encode.list jsonEncProc) val



type Proc  =
    AssertEqual Expr Expr
    | Let String Expr
    | Set String Expr

jsonDecProc : Json.Decode.Decoder ( Proc )
jsonDecProc =
    let jsonDecDictProc = Dict.fromList
            [ ("AssertEqual", Json.Decode.lazy (\_ -> Json.Decode.map2 AssertEqual (Json.Decode.index 0 (jsonDecExpr)) (Json.Decode.index 1 (jsonDecExpr))))
            , ("Let", Json.Decode.lazy (\_ -> Json.Decode.map2 Let (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (jsonDecExpr))))
            , ("Set", Json.Decode.lazy (\_ -> Json.Decode.map2 Set (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (jsonDecExpr))))
            ]
        jsonDecObjectSetProc = Set.fromList []
    in  decodeSumTaggedObject "Proc" "tag" "contents" jsonDecDictProc jsonDecObjectSetProc

jsonEncProc : Proc -> Value
jsonEncProc  val =
    let keyval v = case v of
                    AssertEqual v1 v2 -> ("AssertEqual", encodeValue (Json.Encode.list identity [jsonEncExpr v1, jsonEncExpr v2]))
                    Let v1 v2 -> ("Let", encodeValue (Json.Encode.list identity [Json.Encode.string v1, jsonEncExpr v2]))
                    Set v1 v2 -> ("Set", encodeValue (Json.Encode.list identity [Json.Encode.string v1, jsonEncExpr v2]))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias EnvironmentVars  = (Dict String StringTemplate)

jsonDecEnvironmentVars : Json.Decode.Decoder ( EnvironmentVars )
jsonDecEnvironmentVars =
    Json.Decode.dict (jsonDecStringTemplate)

jsonEncEnvironmentVars : EnvironmentVars -> Value
jsonEncEnvironmentVars  val = (Json.Encode.dict identity (jsonEncStringTemplate)) val



type SceneComputation  =
    SceneNotRun 
    | PrescriptFailed ScriptException
    | RequestFailed HttpException
    | PostscriptFailed ScriptException
    | SceneSucceeded RequestComputationOutput

jsonDecSceneComputation : Json.Decode.Decoder ( SceneComputation )
jsonDecSceneComputation =
    let jsonDecDictSceneComputation = Dict.fromList
            [ ("SceneNotRun", Json.Decode.lazy (\_ -> Json.Decode.succeed SceneNotRun))
            , ("PrescriptFailed", Json.Decode.lazy (\_ -> Json.Decode.map PrescriptFailed (jsonDecScriptException)))
            , ("RequestFailed", Json.Decode.lazy (\_ -> Json.Decode.map RequestFailed (jsonDecHttpException)))
            , ("PostscriptFailed", Json.Decode.lazy (\_ -> Json.Decode.map PostscriptFailed (jsonDecScriptException)))
            , ("SceneSucceeded", Json.Decode.lazy (\_ -> Json.Decode.map SceneSucceeded (jsonDecRequestComputationOutput)))
            ]
    in  decodeSumObjectWithSingleField  "SceneComputation" jsonDecDictSceneComputation

jsonEncSceneComputation : SceneComputation -> Value
jsonEncSceneComputation  val =
    let keyval v = case v of
                    SceneNotRun  -> ("SceneNotRun", encodeValue (Json.Encode.list identity []))
                    PrescriptFailed v1 -> ("PrescriptFailed", encodeValue (jsonEncScriptException v1))
                    RequestFailed v1 -> ("RequestFailed", encodeValue (jsonEncHttpException v1))
                    PostscriptFailed v1 -> ("PostscriptFailed", encodeValue (jsonEncScriptException v1))
                    SceneSucceeded v1 -> ("SceneSucceeded", encodeValue (jsonEncRequestComputationOutput v1))
    in encodeSumObjectWithSingleField keyval val



type PgComputation  =
    PgError String
    | PgCommandOK 
    | PgTuplesOk Table

jsonDecPgComputation : Json.Decode.Decoder ( PgComputation )
jsonDecPgComputation =
    let jsonDecDictPgComputation = Dict.fromList
            [ ("PgError", Json.Decode.lazy (\_ -> Json.Decode.map PgError (Json.Decode.string)))
            , ("PgCommandOK", Json.Decode.lazy (\_ -> Json.Decode.succeed PgCommandOK))
            , ("PgTuplesOk", Json.Decode.lazy (\_ -> Json.Decode.map PgTuplesOk (jsonDecTable)))
            ]
        jsonDecObjectSetPgComputation = Set.fromList []
    in  decodeSumTaggedObject "PgComputation" "tag" "contents" jsonDecDictPgComputation jsonDecObjectSetPgComputation

jsonEncPgComputation : PgComputation -> Value
jsonEncPgComputation  val =
    let keyval v = case v of
                    PgError v1 -> ("PgError", encodeValue (Json.Encode.string v1))
                    PgCommandOK  -> ("PgCommandOK", encodeValue (Json.Encode.list identity []))
                    PgTuplesOk v1 -> ("PgTuplesOk", encodeValue (jsonEncTable v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Table  = (List Column)

jsonDecTable : Json.Decode.Decoder ( Table )
jsonDecTable =
    Json.Decode.list (jsonDecColumn)

jsonEncTable : Table -> Value
jsonEncTable  val = (Json.Encode.list jsonEncColumn) val



type Column  =
    Column String (List PgValue)

jsonDecColumn : Json.Decode.Decoder ( Column )
jsonDecColumn =
    Json.Decode.lazy (\_ -> Json.Decode.map2 Column (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.list (jsonDecPgValue))))


jsonEncColumn : Column -> Value
jsonEncColumn (Column v1 v2) =
    Json.Encode.list identity [Json.Encode.string v1, (Json.Encode.list jsonEncPgValue) v2]



type PgValue  =
    PgString String
    | PgInt Int
    | PgBool Bool
    | PgNull 

jsonDecPgValue : Json.Decode.Decoder ( PgValue )
jsonDecPgValue =
    let jsonDecDictPgValue = Dict.fromList
            [ ("PgString", Json.Decode.lazy (\_ -> Json.Decode.map PgString (Json.Decode.string)))
            , ("PgInt", Json.Decode.lazy (\_ -> Json.Decode.map PgInt (Json.Decode.int)))
            , ("PgBool", Json.Decode.lazy (\_ -> Json.Decode.map PgBool (Json.Decode.bool)))
            , ("PgNull", Json.Decode.lazy (\_ -> Json.Decode.succeed PgNull))
            ]
        jsonDecObjectSetPgValue = Set.fromList []
    in  decodeSumTaggedObject "PgValue" "tag" "contents" jsonDecDictPgValue jsonDecObjectSetPgValue

jsonEncPgValue : PgValue -> Value
jsonEncPgValue  val =
    let keyval v = case v of
                    PgString v1 -> ("PgString", encodeValue (Json.Encode.string v1))
                    PgInt v1 -> ("PgInt", encodeValue (Json.Encode.int v1))
                    PgBool v1 -> ("PgBool", encodeValue (Json.Encode.bool v1))
                    PgNull  -> ("PgNull", encodeValue (Json.Encode.list identity []))
    in encodeSumTaggedObject "tag" "contents" keyval val


postApiRunnerRequestComputation : String -> (TemplatedRequestComputationInput, (Dict String (List Template))) -> (Result Http.Error  ((Either HttpException RequestComputationOutput))  -> msg) -> Cmd msg
postApiRunnerRequestComputation urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "runner"
                    , "requestComputation"
                    ]
                    params
            , body =
                Http.jsonBody ((\(t1,t2) -> Json.Encode.list identity [(jsonEncTemplatedRequestComputationInput) t1,((Json.Encode.dict identity ((Json.Encode.list jsonEncTemplate)))) t2]) body)
            , expect =
                Http.expectJson toMsg ((jsonDecEither jsonDecHttpException) jsonDecRequestComputationOutput)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRunnerScenarioComputation : String -> ScenarioInput -> (Result Http.Error  (ScenarioOutput)  -> msg) -> Cmd msg
postApiRunnerScenarioComputation urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "runner"
                    , "scenarioComputation"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncScenarioInput body)
            , expect =
                Http.expectJson toMsg jsonDecScenarioOutput
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRunnerPgSqlComputation : String -> String -> (Result Http.Error  (PgComputation)  -> msg) -> Cmd msg
postApiRunnerPgSqlComputation urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "runner"
                    , "pgSqlComputation"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson toMsg jsonDecPgComputation
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiRunnerHealth : String -> (Result Http.Error  (())  -> msg) -> Cmd msg
getApiRunnerHealth urlBase toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "runner"
                    , "health"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
