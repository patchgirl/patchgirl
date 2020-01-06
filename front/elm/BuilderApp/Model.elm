module BuilderApp.Model exposing (..)

import Http as Http
import Api.Generated as Client
import Application.Type exposing (..)
import Application.Type as Type
import VarApp.Model as VarApp
import BuilderApp.Builder.Model as Builder

type alias Model a =
    { a
        | selectedBuilderIndex : Maybe Int
        , displayedBuilderIndex : Maybe Int
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuIndex : Maybe Int
        , environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , varAppModel : VarApp.Model
    }

type RequestCollection  =
    RequestCollection Int (List RequestNode)

type RequestNode
    = RequestFolder Folder
    | RequestFile File

type alias Folder =
  { name : Editable String
  , open : Bool
  , children : List RequestNode
  }

type alias File =
  { name : Editable String
  , isSaved : Bool
  , httpUrl : Editable String
  , httpMethod : Editable Builder.Method
  , httpHeaders : Editable (List (String, String))
  , httpBody : Editable String
  , requestComputationResult : Maybe Builder.RequestComputationResult
  , showResponseView : Bool
  }

defaultFolder =
  RequestFolder { name = NotEdited "new folder"
                , open = False
                , children = []
                }

defaultFile =
  RequestFile { name = NotEdited "new file"
              , isSaved = False
              , httpUrl = NotEdited ""
              , httpMethod = NotEdited Builder.Get
              , httpHeaders = NotEdited []
              , httpBody = NotEdited ""
              , showResponseView = False
              , requestComputationResult = Nothing
              }
