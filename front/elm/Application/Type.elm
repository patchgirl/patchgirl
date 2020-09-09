module Application.Type exposing (..)

import Animation
import Dict exposing (Dict)
import Uuid exposing (Uuid)
import Parser exposing(DeadEnd)
import Json.Encode as Json exposing(Value)


-- * menu


type MainMenuName
    = SignInMenu
    | BlogMenu
    | SignOutMenu
    | GithubMenu
    | RunnerStatusMenu


-- * node type


type NodeType a b
    = Folder a
    | File b


-- * builder view


type BuilderView a
    = LandingView WhichDefaultView
    | EditView (WhichEditView a)
    | RunView a

type WhichDefaultView
    = DefaultView
    | CreateDefaultFolderView
    | CreateDefaultFileView

type WhichEditView a
    = DefaultEditView a
    | DeleteView a

mapEditView : (a -> b) -> WhichEditView a -> WhichEditView b
mapEditView f whichEditView =
    case whichEditView of
        DefaultEditView x -> DefaultEditView (f x)
        DeleteView x -> DeleteView (f x)

andThenEditView : WhichEditView a -> (a -> WhichEditView b) -> WhichEditView b
andThenEditView whichEditView f =
    Debug.todo ""

traverseEditViewMaybe : WhichEditView (Maybe a) -> Maybe (WhichEditView a)
traverseEditViewMaybe whichEditView =
    case whichEditView of
        DefaultEditView Nothing -> Nothing
        DeleteView Nothing -> Nothing
        DefaultEditView (Just x) -> Just (DefaultEditView x)
        DeleteView (Just x) -> Just (DeleteView x)


getBuilderId : BuilderView Uuid -> Maybe Uuid
getBuilderId builderView =
    case builderView of
        LandingView _ ->
            Nothing

        EditView whichEditView ->
            Just <|
                case whichEditView of
                    DefaultEditView id -> id
                    DeleteView id -> id

        RunView id -> Just id


-- * notification


type Notification
    = SuccessNotification String String
    | InfoNotification String String
    | AlertNotification String String

notificationEncoder : Notification -> Value
notificationEncoder notification =
    let
        (status, content, debug) =
            case notification of
                SuccessNotification message log ->
                    ("notification", message, log)

                InfoNotification message log ->
                    ("info", message, log)

                AlertNotification message log ->
                    ("alert", message, log)
    in
    Json.object
        [ ( "status", Json.string status )
        , ( "content", Json.string content )
        , ( "debug", Json.string debug )
        ]


-- * session


type Session
    = Visitor VisitorSession
    | SignedUser SignedUserSession


type alias VisitorSession =
    { id : Uuid
    , csrfToken : String
    , signInEmail : String
    , signInPassword : String
    , signInErrors : List String
    , signUpEmail : String
    , signUpError : Maybe String
    , signUpMessage : Maybe String
    }


type alias SignedUserSession =
    { id : Uuid
    , csrfToken : String
    , email : Maybe String
    , avatarUrl : String
    }


getCsrfToken : Session -> String
getCsrfToken session =
    case session of
        Visitor { csrfToken } ->
            csrfToken

        SignedUser { csrfToken } ->
            csrfToken


getSessionId : Session -> Uuid
getSessionId session =
    case session of
        Visitor { id } ->
            id

        SignedUser { id } ->
            id


-- * environment


type alias Environment =
    { id : Uuid
    , name : Editable String
    , showRenameInput : Bool
    , keyValues : List (Storable NewKeyValue KeyValue)
    }

toLatestKeyValue : Storable NewKeyValue KeyValue
                 -> { key : String
                    , value : StringTemplate
                    , hidden : Bool
                    }
toLatestKeyValue storable =
    case storable of
        New { key, value, hidden } ->
            { key = key
            , value = value
            , hidden = hidden
            }

        Saved { key, value, hidden } ->
            { key = key
            , value = value
            , hidden = hidden
            }

        Edited2 _ { key, value, hidden } ->
            { key = key
            , value = value
            , hidden = hidden
            }


-- * key value


type alias NewKeyValue =
    { key : String
    , value : StringTemplate
    , hidden : Bool
    }

type alias KeyValue =
    { id : Uuid
    , key : String
    , value : StringTemplate
    , hidden : Bool
    }


-- * template


type alias StringTemplate = List Template

type Template
  = Sentence String
  | Key String

templateAsString : Template -> String
templateAsString templatedString =
    case templatedString of
        Sentence s -> s
        Key s -> "{{" ++ s ++ "}}"

templatedStringAsString : StringTemplate -> String
templatedStringAsString templatedStrings =
    templatedStrings |> List.map templateAsString |> String.join ""

stringTemplateContainsKey : StringTemplate -> Bool
stringTemplateContainsKey stringTemplate =
    let
        templateIsKey : Template -> Bool
        templateIsKey template =
            case template of
                Key _ -> True
                _ -> False
    in
    List.any templateIsKey stringTemplate


-- * request collection


type RequestCollection
    = RequestCollection Int (List RequestNode)


-- ** request node


type alias RequestNode = NodeType RequestFolderRecord RequestFileRecord


getRequestNodeIdAndName : RequestNode -> { id: Uuid, name: Editable String }
getRequestNodeIdAndName requestNode =
    case requestNode of
        Folder { id, name } -> { id = id, name = name }
        File { id, name } -> { id = id, name = name }


-- ** folder


type alias RequestFolderRecord =
    { id : Uuid
    , name : Editable String
    , open : Bool
    , children : RequestChildren
    }

type RequestChildren = RequestChildren (List RequestNode)


-- ** file


type alias RequestFileRecord =
    { id : Uuid
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List ( String, String ))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationOutput
    , showResponseView : Bool
    , whichResponseView : HttpResponseView
    , runRequestIconAnimation : Animation.State
    }


-- ** which response view


type HttpResponseView
    = BodyResponseView
    | HeaderResponseView


-- ** landing request folder


type alias NewNode =
    { name : String
    , parentFolderId : Maybe Uuid
    }


-- * pg collection


type PgCollection
    = PgCollection Uuid (List PgNode)



-- ** pg node


type alias PgNode = NodeType PgFolderRecord PgFileRecord

getPgNodeIdAndName : PgNode -> { id: Uuid, name: Editable String }
getPgNodeIdAndName pgNode =
    case pgNode of
        Folder { id, name } -> { id = id, name = name }
        File { id, name } -> { id = id, name = name }


-- ** folder


type alias PgFolderRecord =
    { id : Uuid
    , name : Editable String
    , open : Bool
    , children : PgChildren
    }

type PgChildren = PgChildren (List PgNode)


-- ** file


type alias PgFileRecord =
    { id : Uuid
    , name : Editable String
    , dbHost : Editable String
    , dbPassword : Editable String
    , dbPort : Editable String
    , dbUser : Editable String
    , dbName : Editable String
    , sql : Editable String
    , pgComputationOutput : Maybe PgComputationOutput
    , showResponseView : Bool
    }


-- * scenario collection


type ScenarioCollection
    = ScenarioCollection Uuid (List ScenarioNode)


-- ** scenario node


type ScenarioNode
    = ScenarioFolder ScenarioFolderRecord
    | ScenarioFile ScenarioFileRecord



-- ** scenario folder record


type alias ScenarioFolderRecord =
    { id : Uuid
    , name : Editable String
    , children : List ScenarioNode
    , open : Bool
    }



-- ** scenario file record


type alias ScenarioFileRecord =
    { id : Uuid
    , environmentId : Editable (Maybe Uuid)
    , name : Editable String
    , scenes : List Scene
    , whichResponseView : HttpResponseView
    }



-- ** scene


type alias Scene
    = { id : Uuid
      , nodeId : Uuid
      , actorType : ActorType
      , sceneComputation : Maybe SceneComputation
      , prescriptStr : Editable String
      , prescriptAst : Result (List DeadEnd) TangoAst
      , postscriptStr : Editable String
      , postscriptAst : Result (List DeadEnd) TangoAst
      }

type ActorType
    = HttpActor
    | PgActor

type FileRecord
    = HttpRecord RequestFileRecord
    | PgRecord PgFileRecord


-- * builder


type alias Builder =
    { id : Uuid
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List ( String, String ))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationOutput
    , showResponseView : Bool
    , whichResponseView : HttpResponseView
    , requestPending : Bool
    }



-- * request computation


-- ** request computation result


type alias RequestComputationOutput
    = Result HttpException RequestComputation


-- ** http exception


type HttpException
    = InvalidUrlException String String
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


httpExceptionToString : HttpException -> String
httpExceptionToString httpException =
    case httpException of
        InvalidUrlException url _ ->
            "Invalid Url:" ++ url

        TooManyRedirects ->
            "Too Many Redirects"

        OverlongHeaders ->
            "Overlong Headers"

        ResponseTimeout ->
            "Response Timeout"

        ConnectionTimeout ->
            "Connection Timeout"

        ConnectionFailure reason ->
            "Connection Failure: " ++ reason

        InvalidStatusLine ->
            "Invalid Status Line"

        InvalidHeader ->
            "Invalid Header"

        InvalidRequestHeader ->
            "Invalid Request Header"

        InternalException exception ->
            "Internal Exception: " ++ exception

        ProxyConnectException ->
            "Proxy Connect Exception"

        NoResponseDataReceived ->
            "No Response Data Received"

        WrongRequestBodyStreamSize ->
            "Wrong Request Body Stream Size"

        ResponseBodyTooShort ->
            "Response Body Too Short"

        InvalidChunkHeaders ->
            "Invalid Chunk Headers"

        IncompleteHeaders ->
            "Incomplete Headers"

        InvalidDestinationHost ->
            "Invalid Destination Host"

        HttpZlibException ->
            "Http Zlib Exception"

        InvalidProxyEnvironmentVariable ->
            "Invalid Proxy Environment Variable"

        ConnectionClosed ->
            "Connection Closed"

        InvalidProxySettings ->
            "Invalid Proxy Settings"

        UnknownException ->
            "Unknown Exception"



-- ** request computation input


type alias RequestComputationInput =
    { method : HttpMethod
    , headers : List (StringTemplate, StringTemplate)
    , url : StringTemplate
    , body : StringTemplate
    }



-- ** request computation output


type alias RequestComputation =
    { statusCode : Int
    , statusText : String
    , headers : Dict String String
    , body : String
    }



-- ** method


type HttpMethod
    = HttpGet
    | HttpPost
    | HttpPut
    | HttpDelete
    | HttpPatch
    | HttpHead
    | HttpOptions


methodToString : HttpMethod -> String
methodToString method =
    case method of
        HttpGet ->
            "GET"

        HttpPost ->
            "POST"

        HttpPut ->
            "PUT"

        HttpDelete ->
            "DELETE"

        HttpPatch ->
            "PATCH"

        HttpHead ->
            "HEAD"

        HttpOptions ->
            "OPTIONS"


fromString : String -> Maybe HttpMethod
fromString method =
    case method of
        "GET" ->
            Just HttpGet

        "POST" ->
            Just HttpPost

        "PUT" ->
            Just HttpPut

        "DELETE" ->
            Just HttpDelete

        "PATCH" ->
            Just HttpPatch

        "HEAD" ->
            Just HttpHead

        "OPTIONS" ->
            Just HttpOptions

        _ ->
            Nothing



-- ** scheme


type Scheme
    = Http
    | Https

schemeToString : Scheme -> String
schemeToString scheme =
    case scheme of
        Http -> "http"
        Https -> "https"


-- * pg computation


-- ** pg computation output


type alias PgComputationOutput
    = Result String PgComputation


-- ** pg computation


type PgComputation
    = PgCommandOK
    | PgTuplesOk (List Row)


-- ** column


type alias Row = List (String, PgValue)


-- ** pg Value


type PgValue
    = PgString String
    | PgInt Int
    | PgFloat Float
    | PgBool Bool
    | PgNull


-- * scenario computation
-- ** scenario output


type alias ScenarioOutput = List SceneOutput


-- ** scene output


type alias SceneOutput =
    { sceneId : Uuid
    , requestFileNodeId : Uuid
    , sceneComputation : SceneComputation
    }



-- ** scene computation


type SceneComputation
    = SceneNotRun
    | PrescriptFailed ScriptException
    | HttpSceneOk RequestComputation
    | HttpSceneFailed HttpException
    | PgSceneOk PgComputation
    | PgSceneFailed String
    | PgPostscriptFailed PgComputation ScriptException
    | HttpPostscriptFailed RequestComputation ScriptException

type ScriptException
  = UnknownVariable Expr
  | AssertEqualFailed Expr Expr
  | CannotUseFunction String
  | EmptyResponse String
  | AccessOutOfBound
  | CantAccessElem Expr Expr


scriptExceptionToString : ScriptException -> String
scriptExceptionToString scriptException =
    case scriptException of
        UnknownVariable expr ->
            "Unknown variable " ++ exprToString expr

        AssertEqualFailed e1 e2 ->
            "assertEqual failed: " ++ (exprToString e1) ++ " is not equal to " ++ (exprToString e2)

        CannotUseFunction function ->
            "Cannot use function: [" ++ function ++ "]"

        EmptyResponse response ->
            "Empty response: " ++  response

        AccessOutOfBound ->
            "Access out of bound"

        CantAccessElem expr1 expr2 ->
            "Cant access elem: [" ++ exprToString expr1 ++ "]"

-- * editable
{-
   model that don't have a state difference when they are saved or edited
   typically any model that doesn't have an `id` field
-}


type Editable a
    = NotEdited a
    | Edited a a


isDirty : Editable a -> Bool
isDirty editable =
    case editable of
        NotEdited _ ->
            False

        Edited _ _ ->
            True

editedOrNotEditedValue : Editable a -> a
editedOrNotEditedValue editable =
    case editable of
        NotEdited value ->
            value

        Edited _ newValue ->
            newValue

notEditedValue : Editable a -> a
notEditedValue editable =
    case editable of
        NotEdited value ->
            value

        Edited value _ ->
            value

changeEditedValue : a -> Editable a -> Editable a
changeEditedValue newValue editable =
    let
        oldValue =
            notEditedValue editable
    in
    case oldValue == newValue of
        True ->
            NotEdited oldValue

        False ->
            Edited oldValue newValue


-- * storable
{-
   model that have a state difference when they are saved or edited
   typically any model that has an `id` field
-}


type Storable a b
    = New a
    | Saved b
    | Edited2 b b


isStorableDirty : Storable a b -> Bool
isStorableDirty storable =
    case storable of
        New _ ->
            True

        Edited2 _ _ ->
            True

        Saved _ ->
            False

latestValueOfStorable : Storable NewKeyValue KeyValue -> { key : String, value : StringTemplate, hidden : Bool }
latestValueOfStorable storable =
    case storable of
        New { key, value, hidden } -> { key = key, value = value, hidden = hidden }
        Saved { key, value, hidden } -> { key = key, value = value, hidden = hidden }
        Edited2 _ { key, value, hidden } -> { key = key, value = value, hidden = hidden }


-- * tangoscript


-- ** proc


type alias TangoAst = List Proc

type Proc
    = AssertEqual Expr Expr
    | Let String Expr
    | Set String Expr


-- ** expr


type Expr
    = LJson Json
    | LList (List Expr)
    | LBool Bool
    | LInt Int
    | LFloat Float
    | LNull
    | LString String
    | LRowElem (String, Expr)
    -- non primitive type
    | LVar String
    | LFetch String
    | LEq Expr Expr
    | LHttpResponseBodyAsString
    | LHttpResponseStatus
    | LPgSimpleResponse -- results without columnName
    | LPgRichResponse   -- response with columnName
    | LAccessOp Expr Expr


-- ** json


type Json
    = JInt Int
    | JFloat Float
    | JBool Bool
    | JString String
    | JArray (List Json)
    | JObject (Dict String Json)


exprToString : Expr -> String
exprToString expr =
    case expr of
        LBool x ->
            case x of
                True -> "Boolean true"
                False -> "Boolean false"
        LInt x -> "Integer " ++ String.fromInt(x)
        LString x -> "String " ++ x
        LFloat x -> "Float " ++ String.fromFloat(x)
        LNull -> "null"
        LRowElem (key, value) -> "RowElem " ++ key ++ " " ++ exprToString value
        LVar x -> "var " ++ x
        LFetch x -> "global var " ++ x
        LEq e1 e2 -> "Eq " ++ (exprToString e1) ++ " " ++ (exprToString e2)
        LHttpResponseBodyAsString -> "httpResponseBodyAsString"
        LHttpResponseStatus -> "httpResponseStatus"
        LPgSimpleResponse -> "pgResponse"
        LPgRichResponse -> "pgResponseAsArray"
        LList xs ->
            List.map exprToString xs
                |> String.join ", "
                |> \s -> "[" ++ s ++ "]"
        LAccessOp _ _ ->
            "LAccessOp"
        LJson _ ->
            "LJson"
