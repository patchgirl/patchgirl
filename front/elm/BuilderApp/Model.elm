module BuilderApp.Model exposing (..)

import Http as Http
import Api.Client as Client

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
  { name : String
  , open : Bool
  , showRenameInput : Bool
  , children : List RequestNode
  }

type alias File =
  { name : String
  , showRenameInput : Bool
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
  RequestFolder { name = "new folder"
                , open = False
                , children = []
                , showRenameInput = False
                }

defaultFile =
  RequestFile { name = "new file"
              , showRenameInput = False
              , isSaved = False
              , httpUrl = ""
              , httpMethod = Client.Get
              , httpHeaders = []
              , httpBody = ""
              , response = Nothing
              }

emptyBuilderTree =
    [ RequestFolder
          { name = "root"
          , open = False
          , children = []
          , showRenameInput = False
          }
    ]
