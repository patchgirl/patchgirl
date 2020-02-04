module BuilderApp.Model exposing (..)

import Http as Http
import Api.Generated as Client
import Application.Type exposing (..)
import Application.Type as Type
import VarApp.Model as VarApp
import BuilderApp.Builder.Model as Builder
import Uuid
import Random

type alias Model a =
    { a
        | selectedBuilderId : Maybe Uuid.Uuid
        , displayedBuilderIndex : Maybe Int
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid.Uuid
        , environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , varAppModel : VarApp.Model
    }

type RequestCollection  =
    RequestCollection Int (List RequestNode)

getRequestNodeId : RequestNode -> Uuid.Uuid
getRequestNodeId requestNode =
    case requestNode of
        RequestFolder { id } -> id
        RequestFile { id } -> id

type RequestNode
    = RequestFolder Folder
    | RequestFile File

type alias Folder =
  { id: Uuid.Uuid
  , name : Editable String
  , open : Bool
  , children : List RequestNode
  }

type alias File =
  { id: Uuid.Uuid
  , name : Editable String
  , isSaved : Bool
  , httpUrl : Editable String
  , httpMethod : Editable Builder.Method
  , httpHeaders : Editable (List (String, String))
  , httpBody : Editable String
  , requestComputationResult : Maybe Builder.RequestComputationResult
  , showResponseView : Bool
  }
