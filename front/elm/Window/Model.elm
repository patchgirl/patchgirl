module Window.Model exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree
import MainNavBar.Model as MainNavBar
import Postman.Model as Postman
import RequestRunner.Model as RequestRunner
import EnvApp.Model as EnvApp
import EnvApp.EnvNav.Model as EnvNav
import VarApp.Model as VarApp
import BuilderApp.EnvSelection.Model as EnvSelection
import BuilderApp.Model as BuilderApp
import WorkspaceApp.Model as WorkspaceApp

type alias Model =
  { mainNavBarModel : MainNavBar.Model
  , buildersAppModel : List BuilderApp.Model
  , selectedWorkspaceIndex : Maybe Int
  , workspaces : List WorkspaceApp.Model
  , postmanModel : Postman.Model
  , envModel : EnvApp.Model
  , selectedEnvironmentToRunIndex : Maybe Int
  , selectedEnvironmentToEditIndex : Maybe Int
  , envNavModel : EnvNav.Model
  , varAppModel : VarApp.Model
  , runnerModel : RequestRunner.Model
  }

getWorkspaceNames : Model -> List String
getWorkspaceNames model =
    List.map .name model.workspaces

getEnvironmentNames : Model -> List String
getEnvironmentNames model =
    List.map .name model.envNavModel.envs

defaultModel : Model
defaultModel =
  let
      selectedWorkspaceIndex = Just 0
      workspaces = WorkspaceApp.defaultModel
      buildersAppModel = List.map .builder workspaces
      envModel : EnvApp.Model
      envModel = [("url", "swapi.co")]
      envNav1 : EnvNav.EnvInfo
      envNav1 =
          { name = "env1"
          , env = [("url", "swapi.co")]
          }
      envNav2 : EnvNav.EnvInfo
      envNav2 =
          { name = "env2"
          , env = [("url", "url2.com")]
          }
      envNav3 : EnvNav.EnvInfo
      envNav3 =
          { name = "env3"
          , env = [("url", "url3.com")]
          }
      envNavModel : EnvNav.Model
      envNavModel =
          { selectedEnvIndex = Just 0
          , renameEnvIdx = Nothing
          , envs = [ envNav1, envNav2, envNav3 ]
          }
      selectedEnvModel : EnvSelection.Model
      selectedEnvModel =
          { envs = [ envNav1.name, envNav2.name, envNav3.name ]
          , selectedEnvIdx = Nothing
          }
      varAppModel =
          { vars =
                [ ("key0", "value0")
                , ("key1", "value1")
                , ("key2", "value2")
                , ("key3", "value3")
                , ("key4", "value4")
                ]
          , overZoneId = Nothing
          , draggedId = Nothing
          }
      selectedEnvironmentToEditIndex = Just 0
      selectedEnvironmentToRunIndex = Just 0
  in
      { mainNavBarModel = MainNavBar.defaultModel
      , buildersAppModel = buildersAppModel
      , selectedWorkspaceIndex = selectedWorkspaceIndex
      , workspaces = workspaces
      , postmanModel = Nothing
      , envModel = envModel
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditIndex = selectedEnvironmentToEditIndex
      , envNavModel = envNavModel
      , runnerModel = Nothing
      , varAppModel = varAppModel
      }
