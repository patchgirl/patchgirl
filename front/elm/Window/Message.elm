module Window.Message exposing (..)

import Http

import BuilderApp.Message as BuilderApp
import BuilderApp.Builder.Message as BuilderAppBuilder
import BuilderTree.Message as BuilderTree
import Postman.Message as Postman
import EnvApp.Message as EnvApp
import EnvNav.Message as EnvNav
import RequestRunner.Message as RequestRunner
import MainNavBar.Message as MainNavBar
import EnvSelection.Message as EnvSelection
import VarApp.Message as VarApp

type Msg
  = BuilderTreeMsg BuilderTree.Msg
  | BuilderMsg BuilderAppBuilder.Msg
  | BuilderAppMsg BuilderApp.Msg
  | PostmanMsg Postman.Msg
  | EnvAppMsg EnvApp.Msg
  | EnvNavMsg EnvNav.Msg
  | RequestRunnerMsg RequestRunner.Msg
  | MainNavBarMsg MainNavBar.Msg
  | EnvSelectionMsg EnvSelection.Msg
  | VarAppMsg VarApp.Msg
  | SaveBuilderTreeResponse (Result Http.Error String)
