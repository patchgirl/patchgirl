module BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree

type alias Model =
    { builderTreeModel : BuilderTree.Model
    }
