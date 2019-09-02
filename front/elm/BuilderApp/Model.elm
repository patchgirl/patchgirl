module BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree

-- TODO remove alias and put directly BuilderTree.Model as the Model
type alias Model =
    { builderTreeModel : BuilderTree.Model
    }

defaultModel =
    { builderTreeModel =
          { selectedBuilderIndex = Just 4
          , displayedBuilderIndex = Just 4
          , tree = BuilderTree.defaultBuilderTree
          , displayedNodeMenuIndex = Nothing
          }
    }

emptyModel =
    { builderTreeModel =
          { selectedBuilderIndex = Nothing
          , displayedBuilderIndex = []
          , tree = BuilderTree.emptyBuilderTree
          , displayedNodeMenuIndex = Nothing
          }
    }
