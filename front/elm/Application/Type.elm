module Application.Type exposing (..)

import Uuid
import Dict
import Animation

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
    { id: Uuid.Uuid
    , csrfToken: String
    , signInEmail: String
    , signInPassword: String
    , signInErrors: List String
    , signUpEmail: String
    , signUpError: Maybe String
    , signUpMessage: Maybe String
    }

type alias SignedUserSession =
    { id: Uuid.Uuid
    , csrfToken: String
    , email: String
    , avatarUrl : String
    }

getCsrfToken : Session -> String
getCsrfToken session =
    case session of
        Visitor { csrfToken } -> csrfToken
        SignedUser { csrfToken } -> csrfToken

getSessionId : Session -> Uuid.Uuid
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
  , httpUrl : Editable String
  , httpMethod : Editable HttpMethod
  , httpHeaders : Editable (List (String, String))
  , httpBody : Editable String
  , requestComputationResult : Maybe RequestComputationResult
  , showResponseView : Bool
  , runRequestIconAnimation : Animation.State
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
    , requestPending : Bool
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
        oldValue = notEditedValue editable
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
