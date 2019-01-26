module Tree.Model exposing (..)

import Builder.Model as Builder

type Node
  =
    Folder
      { name : String
      , open : Bool
      , showRenameInput : Bool
      , children : Tree
      }
    | File
      { name : String
      , showRenameInput : Bool
      , builder : Builder.Model
      }

type alias Tree = List(Node)

type alias Model =
  { selectedNode : Maybe Node
  , displayedBuilderIndex : Maybe Int
  , tree : Tree
  }


defaultTree =
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
                                                     }
                                              , File { name = "file2"
                                                     , builder = Builder.defaultModel2
                                                     , showRenameInput = False
                                                     }
                                              ]
                                 }
                        ]
           }
  ]
