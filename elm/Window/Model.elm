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
      { selectedBuilderIndex = Just 4
      , displayedBuilderIndexes = [4]
      , tree = Tree.defaultTree
      }
    envNav1 =
      { name = "env1"
      , env = [("url", "swapi.co")]
      }
    envNav2 =
      { name = "env2"
      , env = [("url2", "swapi.co")]
      }
    envNavModel =
      { selectedEnvIndex = Just 0
      , renameEnvIdx = Nothing
      , envs = [ envNav1, envNav2 ]
      }
  in
    { tabModel = Tab.defaultModel
    , treeModel = treeModel
    , postmanModel = Nothing
    , envModel = [("url", "swapi.co")]
    , envNavModel = envNavModel
    , runnerModel = Nothing
    }
