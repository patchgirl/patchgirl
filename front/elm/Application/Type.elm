module Application.Type exposing (..)

import Animation
import Dict
import Uuid exposing (Uuid)



-- * menu


type MainMenuName
    = SignInMenu
    | BlogMenu
    | SignOutMenu
    | GithubMenu



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
    , email : String
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
    { id : Int
    , name : Editable String
    , showRenameInput : Bool
    , keyValues : List (Storable NewKeyValue KeyValue)
    }



-- * key value


type alias NewKeyValue =
    { key : String
    , value : String
    }


type alias KeyValue =
    { id : Int
    , key : String
    , value : String
    }



-- * request collection


type RequestCollection
    = RequestCollection Int (List RequestNode)



-- ** request node


type RequestNode
    = RequestFolder RequestFolderRecord
    | RequestFile RequestFileRecord



-- ** folder


type alias RequestFolderRecord =
    { id : Uuid
    , name : Editable String
    , open : Bool
    , children : List RequestNode
    }



-- ** file


type alias RequestFileRecord =
    { id : Uuid
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List ( String, String ))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationResult
    , showResponseView : Bool
    , whichResponseView : HttpResponseView
    , runRequestIconAnimation : Animation.State
    }


-- ** which response view


type HttpResponseView
    = BodyResponseView
    | HeaderResponseView


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
    , name : Editable String
    , scenes : List Scene
    , showDetailedSceneView : Maybe Uuid
    }



-- ** scene


type alias Scene =
    { id : Uuid
    , requestFileNodeId : Uuid
    , computationOutput : Maybe SceneComputation
    }



-- * builder


type alias Builder =
    { id : Uuid
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List ( String, String ))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationResult
    , showResponseView : Bool
    , whichResponseView : HttpResponseView
    , requestPending : Bool
    }



-- * request computation
-- ** request computation result


type RequestComputationResult
    = RequestComputationFailed HttpException
    | RequestComputationSucceeded RequestComputationOutput



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
    | InternalException
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

        InternalException ->
            "Internal Exception"

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
    { scheme : Scheme
    , method : HttpMethod
    , headers : List ( String, String )
    , url : String
    , body : String
    }



-- ** request computation output


type alias RequestComputationOutput =
    { statusCode : Int
    , statusText : String
    , headers : Dict.Dict String String
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


-- * scenario computation
-- ** scenario computation output


type alias ScenarioComputationOutput =
    { scenarioId : Uuid
    , scenes : List OutputScene
    }



-- ** output scene


type alias OutputScene =
    { sceneId : Uuid
    , requestFileNodeId : Uuid
    , requestComputationOutput : SceneComputation
    }



-- ** scene computation


type SceneComputation
    = SceneRun RequestComputationResult
    | SceneNotRun



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
