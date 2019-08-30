module Window.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.BuilderTree.View as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import Postman.View as Postman
import EnvironmentKeyValueEdition.View as EnvironmentKeyValueEdition
import EnvironmentEdition.View as EnvironmentEdition
import EnvironmentToRunSelection.App as EnvSelection
import BuilderApp.WorkspaceSelection.App as WorkspaceSelection
import MainNavBar.View as MainNavBar
import MainNavBar.Model as MainNavBar
import VarApp.View as VarApp
import WorkspaceApp.View as WorkspaceApp

import BuilderApp.Model as BuilderApp
import BuilderApp.Builder.View as Builder
import BuilderApp.View as BuilderApp

import Util.List as List
import List.Extra as List
import BuilderApp.BuilderTree.Model as BuilderTree

import Window.Model exposing(..)
import Window.Message exposing(..)

view : Model -> Html Msg
view model =
    let
        contentView : Html Msg
        contentView =
            div [ id "content" ] <|
                case model.mainNavBarModel of
                    MainNavBar.ReqTab -> [ builderView model ]
                    MainNavBar.EnvTab -> [ Html.map EnvironmentEditionMsg (EnvironmentEdition.view model) ]
                    MainNavBar.VarTab -> [ Html.map VarAppMsg (VarApp.view model.varAppModel) ]
                    MainNavBar.WorkspaceTab -> [ Html.map WorkspaceAppMsg (WorkspaceApp.view model.workspaces) ]
    in
        div []
            [ Html.map MainNavBarMsg (MainNavBar.view model.mainNavBarModel)
            , contentView
            ]

builderView : Model -> Html Msg
builderView model =
  let
    builderApp : BuilderApp.Model
    builderApp =
        case Maybe.andThen (\idx -> List.getAt idx model.buildersAppModel) <| model.selectedWorkspaceIndex of
            Just builder -> builder
            Nothing -> Debug.log "could not find builderApp, this should never happen" BuilderApp.defaultModel
    treeView : Html Msg
    treeView =
      Html.map BuilderTreeMsg (BuilderTree.view builderApp.builderTreeModel)
    envSelectionView : Html Msg
    envSelectionView =
        Html.map EnvSelectionMsg (EnvSelection.view (List.map .name model.environments))
    workspaceSelectionView : Html Msg
    workspaceSelectionView =
      Html.map WorkspaceSelectionMsg (WorkspaceSelection.view model (getWorkspaceNames model))
  in
    div [ id "builderApp" ]
      [ div [ id "treeView" ] [ treeView ]
      , div [ id "builderView" ] [ (Html.map BuilderAppMsg (BuilderApp.view builderApp)) ]
      , div [ class "" ] [ envSelectionView ]
      , div [ class "" ] [ workspaceSelectionView ]
      ]

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view
