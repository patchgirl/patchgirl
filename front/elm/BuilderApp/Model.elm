module BuilderApp.Model exposing (..)

import Http as Http
import Api.Client as Client
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
  , httpMethod : Client.Method
  , httpHeaders : Editable (List (String, String))
  , httpBody : Editable String
  , response : Maybe Builder.Response
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
              , httpMethod = Client.Get
              , httpHeaders = NotEdited []
              , httpBody = NotEdited ""
              , response = Nothing
              , showResponseView = False
              }

emptyBuilderTree =
    [ RequestFolder
          { name = NotEdited "root"
          , open = False
          , children = []
          }
    ]
