module BuilderApp.Model exposing (..)

import BuilderApp.Builder.Model as Builder

type alias Model a =
    { a
        | selectedBuilderIndex : Maybe Int
        , displayedBuilderIndex : Maybe Int
        , tree : List RequestNode
        , displayedRequestNodeMenuIndex : Maybe Int
    }

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
  , builder : Builder.Model
  }

defaultModel =
    { selectedBuilderIndex = Just 4
    , displayedBuilderIndex = Just 4
    , tree = defaultBuilderTree
    , displayedRequestNodeMenuIndex = Nothing
    }

emptyModel =
    { selectedBuilderIndex = Nothing
    , displayedBuilderIndex = []
    , tree = []
    , displayedRequestNodeMenuIndex = Nothing
    }

defaultFolder =
  RequestFolder { name = "new folder"
                , open = False
                , children = []
                , showRenameInput = False
                }

defaultFile =
  RequestFile { name = "new file"
              , builder = Builder.defaultBuilder
              , showRenameInput = False
              , isSaved = False
              }

defaultBuilderTree =
  [ RequestFolder
        { name = "folder1"
        , open = False
        , children = []
        , showRenameInput = False
        }
  , RequestFolder
        { name = "folder2"
        , open = True
        , showRenameInput = False
        , children =
              [ RequestFolder
                    { name = "folder2.2"
                    , open = True
                    , children = []
                    , showRenameInput = False
                    }
              , RequestFolder
                    { name = "folder3"
                    , open = True
                    , showRenameInput = False
                    , children =
                          [ RequestFile
                                { name = "file1"
                                , builder = Builder.defaultModel1
                                , showRenameInput = False
                                , isSaved = False
                                }
                          , RequestFile
                                { name = "file2"
                                , builder = Builder.defaultModel
                                , showRenameInput = False
                                , isSaved = True
                                }
                          ]
                    }
              ]
        }
  ]

emptyBuilderTree =
    [ RequestFolder
          { name = "root"
          , open = False
          , children = []
          , showRenameInput = False
          }
    ]
