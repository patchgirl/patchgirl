module Window.Model exposing (..)

import Tree.Model as Tree
import Tab.Model as Tab
import Postman.Model as Postman
import Runner.Model as Runner
import Env.Model as Env
import EnvNav.Model as EnvNav
import EnvSelection.Model as EnvSelection

type alias Model =
  { tabModel : Tab.Model
  , treeModel : Tree.Model
  , postmanModel : Postman.Model
  , envModel : Env.Model
  , selectedEnvModel : EnvSelection.Model
  , envNavModel : EnvNav.Model
  , runnerModel : Runner.Model
  }

defaultModel : Model
defaultModel =
  let
    treeModel =
      { selectedBuilderIndex = Just 4
      , displayedBuilderIndexes = [4]
      , tree = Tree.defaultTree
      }
    envModel : Env.Model
    envModel = [("url", "swapi.co")]
    envNav1 : EnvNav.EnvInfo
    envNav1 =
      { name = "env1"
      , env = [("url", "swapi.co")]
      }
    envNav2 : EnvNav.EnvInfo
    envNav2 =
      { name = "env2"
      , env = [("url2", "swapi.co")]
      }
    envNavModel : EnvNav.Model
    envNavModel =
      { selectedEnvIndex = Just 0
      , renameEnvIdx = Nothing
      , envs = [ envNav1, envNav2 ]
      }
    selectedEnvModel : EnvSelection.Model
    selectedEnvModel =
      { envs = [ envNav1.name, envNav2.name ]
      , selectedEnvIdx = Nothing
      }
  in
    { tabModel = Tab.defaultModel
    , treeModel = treeModel
    , postmanModel = Nothing
    , envModel = envModel
    , selectedEnvModel = selectedEnvModel
    , envNavModel = envNavModel
    , runnerModel = Nothing
    }
