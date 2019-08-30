module Window.Message exposing (..)

import Http

import BuilderApp.Message as BuilderApp
import BuilderApp.Builder.Message as BuilderAppBuilder
import BuilderApp.BuilderTree.Message as BuilderTree
import Postman.Message as Postman
import EnvironmentKeyValueEdition.Message as EnvironmentKeyValueEdition
import EnvironmentEdition.Message as EnvironmentEdition
import RequestRunner.Message as RequestRunner
import MainNavBar.Message as MainNavBar
import EnvironmentToRunSelection.Message as EnvSelection
import BuilderApp.WorkspaceSelection.Message as WorkspaceSelection
import VarApp.Message as VarApp
import WorkspaceApp.Message as WorkspaceApp

type Msg
  = BuilderTreeMsg BuilderTree.Msg
  | BuilderAppMsg BuilderApp.Msg
  | PostmanMsg Postman.Msg
  | EnvironmentKeyValueEditionMsg EnvironmentKeyValueEdition.Msg
  | EnvironmentEditionMsg EnvironmentEdition.Msg
  | RequestRunnerMsg RequestRunner.Msg
  | MainNavBarMsg MainNavBar.Msg
  | EnvSelectionMsg EnvSelection.Msg
  | VarAppMsg VarApp.Msg
  | SaveBuilderTreeResponse (Result Http.Error String)
  | WorkspaceAppMsg WorkspaceApp.Msg
  | WorkspaceSelectionMsg WorkspaceSelection.Msg
