module Window.Message exposing (..)

import Http

import BuilderApp.Message as BuilderApp
import BuilderApp.Builder.Message as BuilderAppBuilder
import BuilderApp.BuilderTree.Message as BuilderTree
import Postman.Message as Postman
import EnvApp.Message as EnvApp
import EnvApp.EnvNav.Message as EnvNav
import RequestRunner.Message as RequestRunner
import MainNavBar.Message as MainNavBar
import BuilderApp.EnvSelection.Message as EnvSelection
import BuilderApp.WorkspaceSelection.Message as WorkspaceSelection
import VarApp.Message as VarApp
import WorkspaceApp.Message as WorkspaceApp

type Msg
  = BuilderTreeMsg BuilderTree.Msg
  | BuilderAppMsg BuilderApp.Msg
  | PostmanMsg Postman.Msg
  | EnvAppMsg EnvApp.Msg
  | EnvNavMsg EnvNav.Msg
  | RequestRunnerMsg RequestRunner.Msg
  | MainNavBarMsg MainNavBar.Msg
  | EnvSelectionMsg EnvSelection.Msg
  | VarAppMsg VarApp.Msg
  | SaveBuilderTreeResponse (Result Http.Error String)
  | WorkspaceAppMsg WorkspaceApp.Msg
  | WorkspaceSelectionMsg WorkspaceSelection.Msg
