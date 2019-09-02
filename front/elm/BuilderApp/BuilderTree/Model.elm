module BuilderApp.BuilderTree.Model exposing (..)

import BuilderApp.Builder.Model as Builder

type alias Model =
  { selectedBuilderIndex : Maybe Int
  , displayedBuilderIndex : Maybe Int
  , tree : BuilderTree
  , displayedNodeMenuIndex : Maybe Int
  }

type alias Folder2 =
  { name : String
  , open : Bool
  , showRenameInput : Bool
  , children : BuilderTree
  }

type alias File2 =
  { name : String
  , showRenameInput : Bool
  , isSaved : Bool
  , builder : Builder.Model
  }

type Node = Folder Folder2 | File File2

type alias BuilderTree = List(Node)

defaultFolder =
  Folder { name = "new folder"
         , open = False
         , children = []
         , showRenameInput = False
         }

defaultFile =
  File { name = "new file"
       , builder = Builder.defaultBuilder
       , showRenameInput = False
       , isSaved = False
       }

defaultBuilderTree =
  [ Folder { name = "folder1"
           , open = False
           , children = []
           , showRenameInput = False
           }
  , Folder { name = "folder2"
           , open = True
           , showRenameInput = False
           , children = [ Folder { name = "folder2.2"
                                 , open = True
                                 , children = []
                                 , showRenameInput = False
                                 }
                        , Folder { name = "folder3"
                                 , open = True
                                 , showRenameInput = False
                                 , children = [ File { name = "file1"
                                                     , builder = Builder.defaultModel1
                                                     , showRenameInput = False
                                                     , isSaved = False
                                                     }
                                              , File { name = "file2"
                                                     , builder = Builder.defaultModel2
                                                     , showRenameInput = False
                                                     , isSaved = True
                                                     }
                                              ]
                                 }
                        ]
           }
  ]

emptyBuilderTree =
    [ Folder
          { name = "root"
          , open = False
          , children = []
          , showRenameInput = False
          }
    ]
