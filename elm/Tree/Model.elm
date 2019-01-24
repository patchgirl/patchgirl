module Tree.Model exposing (..)

import Builder.Model as Builder

type Node = Folder String Bool Tree | File String Builder.Model
type alias Tree = List(Node)

type alias Model =
  { selectedNode : Maybe Node
  , displayedBuilderIndex : Maybe Int
  , tree : Tree
  }
