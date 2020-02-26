module Application.Type exposing (..)

import Uuid
import Dict


-- * session


type Session
    = Visitor VisitorSession
    | SignedUser { id: Int
                 , csrfToken: String
                 , email: String
                 }

type alias VisitorSession =
    { id: Int
    , csrfToken: String
    , signInEmail: String
    , signInPassword: String
    , signInErrors: List String
    , signUpEmail: String
    , signUpError: Maybe String
    , signUpMessage: Maybe String
    }

getCsrfToken : Session -> String
getCsrfToken session =
    case session of
        Visitor { csrfToken } -> csrfToken
        SignedUser { csrfToken } -> csrfToken

getSessionId : Session -> Int
getSessionId session =
    case session of
        Visitor { id } -> id
        SignedUser { id } -> id


-- * sign up


type alias SignUp =
    { email : String
    }


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


type RequestCollection  =
    RequestCollection Int (List RequestNode)


-- ** request node


type RequestNode
    = RequestFolder Folder
    | RequestFile File


-- ** folder


type alias Folder =
  { id: Uuid.Uuid
  , name : Editable String
  , open : Bool
  , children : List RequestNode
  }


-- ** file


type alias File =
  { id: Uuid.Uuid
  , name : Editable String
  , isSaved : Bool
  , httpUrl : Editable String
  , httpMethod : Editable HttpMethod
  , httpHeaders : Editable (List (String, String))
  , httpBody : Editable String
  , requestComputationResult : Maybe RequestComputationResult
  , showResponseView : Bool
  }

-- * initialize password state


type InitializePasswordState
    = InitialPasswordState
    | FilledPasswordState
    | FailedPasswordState String
    | SucceededPasswordState


-- * builder


type alias Builder =
    { id: Uuid.Uuid
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List (String, String))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationResult
    , showResponseView : Bool
    }


-- * http

-- ** request computation result


type RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotRequestComputationOutput RequestComputationOutput


-- ** request computation input


type alias RequestComputationInput =
    { scheme : Scheme
    , method : HttpMethod
    , headers : List (String, String)
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
    HttpGet -> "GET"
    HttpPost -> "POST"
    HttpPut -> "PUT"
    HttpDelete -> "DELETE"
    HttpPatch -> "PATCH"
    HttpHead -> "HEAD"
    HttpOptions -> "OPTIONS"

fromString : String -> Maybe HttpMethod
fromString method =
  case method of
    "GET" -> Just HttpGet
    "POST" -> Just HttpPost
    "PUT" -> Just HttpPut
    "DELETE" -> Just HttpDelete
    "PATCH" -> Just HttpPatch
    "HEAD" -> Just HttpHead
    "OPTIONS" -> Just HttpOptions
    _ -> Nothing


-- ** scheme


type Scheme
    = Http
    | Https


-- * editable


type Editable a
    = NotEdited a
    | Edited a a

type Storable a b
    = New a
    | Saved b
    | Edited2 b b

isDirty : Editable a -> Bool
isDirty editable =
    case editable of
        NotEdited _ ->
            False

        Edited _ _ ->
            True

isDirty2 : Storable a b -> Bool
isDirty2 storable =
    case storable of
        New _ ->
            True

        Edited2 _ _ ->
            True

        Saved _ ->
            False


editedOrNotEditedValue : Editable a -> a
editedOrNotEditedValue editable =
    case editable of
        NotEdited value ->
            value
        Edited _ newValue ->
            newValue

storedOrNotStoredValue : Storable a b -> (a -> c) -> (b -> c) -> c
storedOrNotStoredValue storable f g =
    case storable of
        New value ->
            f value

        Saved value ->
            g value

        Edited2 _ value ->
            g value

notEditedValue : Editable a -> a
notEditedValue editable =
    case editable of
        NotEdited value ->
            value
        Edited value _ ->
            value

editedValue : Editable a -> Maybe a
editedValue editable =
    case editable of
        NotEdited _ -> Nothing
        Edited _ value -> Just value

changeEditedValue : a -> Editable a -> Editable a
changeEditedValue newValue editable =
    let
        oldValue = notEditedValue editable
    in
        case oldValue == newValue of
            True ->
                NotEdited oldValue

            False ->
                Edited oldValue newValue

changeEditedValue2 : Editable a -> Editable a -> Editable a
changeEditedValue2 eOldValue eNewValue =
    let
        oldValue = notEditedValue eOldValue
        newValue = editedOrNotEditedValue eNewValue
    in
        case oldValue == newValue of
        True ->
            NotEdited oldValue

        False->
            eNewValue
