module InitializedApplication.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Element as UI

import BuilderApp.BuilderTree.View as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import Postman.View as Postman
import EnvironmentKeyValueEdition.View as EnvironmentKeyValueEdition
import EnvironmentEdition.View as EnvironmentEdition
import EnvironmentToRunSelection.App as EnvSelection
import MainNavBar.View as MainNavBar
import MainNavBar.Model as MainNavBar
import VarApp.View as VarApp

import BuilderApp.Model as BuilderApp
import BuilderApp.Builder.View as Builder
import BuilderApp.View as BuilderApp

import Util.List as List
import List.Extra as List

import InitializedApplication.Model exposing(..)
import InitializedApplication.Message exposing(..)

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
    in
        UI.layout [ UI.explain Debug.todo ] <|
            UI.column [ UI.width UI.fill, UI.centerY, UI.spacing 30 ]
                [ UI.map MainNavBarMsg (MainNavBar.view model.mainNavBarModel)
                , UI.html contentView
                ]

builderView : Model -> Html Msg
builderView model =
  let
    envSelectionView : Html Msg
    envSelectionView =
        Html.map EnvSelectionMsg (EnvSelection.view (List.map .name model.environments))
  in
    div [ id "builderApp" ]
      [ div [ id "builderView" ] [ (Html.map BuilderAppMsg (BuilderApp.view model)) ]
      , div [ class "" ] [ envSelectionView ]
      ]

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view
