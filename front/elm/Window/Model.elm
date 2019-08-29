module Window.Model exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree
import MainNavBar.Model as MainNavBar
import Postman.Model as Postman
import RequestRunner.Model as RequestRunner
import EnvironmentKeyValueEdition.Model as EnvironmentKeyValueEdition
import EnvironmentKeyValueEdition.EnvNav.Util as EnvNav
import VarApp.Model as VarApp
import BuilderApp.EnvSelection.Model as EnvSelection
import BuilderApp.Model as BuilderApp
import WorkspaceApp.Model as WorkspaceApp
import List.Extra as List
import Window.Type as Type

type alias Model =
  { mainNavBarModel : MainNavBar.Model
  , buildersAppModel : List BuilderApp.Model
  , selectedWorkspaceIndex : Maybe Int
  , workspaces : List WorkspaceApp.Model
  , postmanModel : Postman.Model
  , selectedEnvironmentToRunIndex : Maybe Int
  , selectedEnvironmentToEditIndex : Maybe Int
  , selectedEnvironmentToRenameIndex : Maybe Int
  , environments : List Type.Environment
  , envModel : EnvironmentKeyValueEdition.Model
  , varAppModel : VarApp.Model
  , runnerModel : RequestRunner.Model
  }

getWorkspaceNames : Model -> List String
getWorkspaceNames model =
    List.map .name model.workspaces

getEnvironmentNames : Model -> List String
getEnvironmentNames model =
    List.map .environmentName model.environments

getEnvironmentToRun : Model -> Maybe Type.Environment
getEnvironmentToRun model =
    let
        selectEnvironment : Int -> Maybe Type.Environment
        selectEnvironment idx = List.getAt idx model.environments
    in
        Maybe.andThen selectEnvironment model.selectedEnvironmentToRunIndex

getEnvironmentKeyValuesToRun : Model -> List(String, String)
getEnvironmentKeyValuesToRun model =
    (getEnvironmentToRun model) |> Maybe.map .keyValues |> Maybe.withDefault []

getEnvironmentKeyValuesToEdit : Model -> List(String, String)
getEnvironmentKeyValuesToEdit model =
    (EnvNav.getEnvironmentToEdit model) |> Maybe.map .keyValues |> Maybe.withDefault []

defaultModel : Model
defaultModel =
  let
      selectedWorkspaceIndex = Just 0
      workspaces = WorkspaceApp.defaultModel
      buildersAppModel = List.map .builder workspaces
      envModel : EnvironmentKeyValueEdition.Model
      envModel = [("url", "swapi.co")]
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
      selectedEnvironmentToRenameIndex = Nothing
      environments =
          [ { environmentName = "staging1"
            , keyValues = [("key1", "value1")]
            }
          , { environmentName = "staging2"
            , keyValues = []
            }
          ]
  in
      { mainNavBarModel = MainNavBar.defaultModel
      , buildersAppModel = buildersAppModel
      , selectedWorkspaceIndex = selectedWorkspaceIndex
      , workspaces = workspaces
      , postmanModel = Nothing
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditIndex = selectedEnvironmentToEditIndex
      , selectedEnvironmentToRenameIndex = selectedEnvironmentToRenameIndex
      , environments = environments
      , envModel = envModel
      , runnerModel = Nothing
      , varAppModel = varAppModel
      }
