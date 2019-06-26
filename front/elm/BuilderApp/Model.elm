module BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree

type alias Model =
    { builderTreeModel : BuilderTree.Model
    }

defaultModel =
    { builderTreeModel =
          { selectedBuilderIndex = Just 4
          , displayedBuilderIndexes = [4, 5]
          , tree = BuilderTree.defaultBuilderTree
          , displayedNodeMenuIndex = Nothing
          }
    }

emptyModel =
    { builderTreeModel =
          { selectedBuilderIndex = Nothing
          , displayedBuilderIndexes = []
          , tree = BuilderTree.emptyBuilderTree
          , displayedNodeMenuIndex = Nothing
          }
    }
