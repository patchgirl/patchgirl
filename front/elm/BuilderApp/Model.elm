module BuilderApp.Model exposing (..)

import Http as Http
import Api.Client as Client
import Application.Type exposing (..)

type alias Model a =
    { a
        | selectedBuilderIndex : Maybe Int
        , displayedBuilderIndex : Maybe Int
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuIndex : Maybe Int
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
  , httpUrl : String
  , httpMethod : Client.Method
  , httpHeaders : List Header
  , httpBody : String
  , response : Maybe Response
  }

type alias Response = Result Http.Error String
type alias Header = (String, String)

defaultFolder =
  RequestFolder { name = NotEdited "new folder"
                , open = False
                , children = []
                }

defaultFile =
  RequestFile { name = NotEdited "new file"
              , isSaved = False
              , httpUrl = ""
              , httpMethod = Client.Get
              , httpHeaders = []
              , httpBody = ""
              , response = Nothing
              }

emptyBuilderTree =
    [ RequestFolder
          { name = NotEdited "root"
          , open = False
          , children = []
          }
    ]
