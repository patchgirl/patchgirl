module BuilderApp.Model exposing (..)

import Http as Http

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
  , url : String
  , method : Method
  , headers : List(Header)
  , body : String
  , response : Maybe Response
  }

type alias Response = Result Http.Error String
type Method = Get | Post | Put | Delete | Patch | Head | Options
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
              , url = ""
              , method = Get
              , headers = []
              , body = ""
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
