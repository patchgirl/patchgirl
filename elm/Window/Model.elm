module Window.Model exposing (..)

import Tree.Model as Tree
import Tab.Model as Tab
import Postman.Model as Postman
import Runner.Model as Runner
import Env.Model as Env
import EnvNav.Model as EnvNav

type alias Model =
  { tabModel : Tab.Model
  , treeModel : Tree.Model
  , postmanModel : Postman.Model
  , envModel : Env.Model
  , envNavModel : EnvNav.Model
  , runnerModel : Runner.Model
  }

defaultModel =
  let
    treeModel =
      { selectedNode = Nothing
      , displayedBuilderIndexes = [4]
      , tree = Tree.defaultTree
      }
  in
    { tabModel = Tab.defaultModel
    , treeModel = treeModel
    , postmanModel = Nothing
    , envModel = [("url", "swapi.co")]
    , envNavModel = ["env1"]
    , runnerModel = Nothing
    }
