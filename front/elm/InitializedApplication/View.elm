module InitializedApplication.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)

import BuilderApp.BuilderTree.View as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import Postman.View as Postman
import EnvironmentEdition.App as EnvironmentEdition
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
        contentView : Element Msg
        contentView =
            el [ width fill ] <|
                case model.mainNavBarModel of
                    MainNavBar.ReqTab -> builderView model
                    MainNavBar.EnvTab -> map EnvironmentEditionMsg (EnvironmentEdition.view model)
                    MainNavBar.VarTab -> builderView model --[ Html.map VarAppMsg (VarApp.view model.varAppModel) ]
    in
        layout [] <|
            column [ width fill, centerY, spacing 30 ]
                [ map MainNavBarMsg (MainNavBar.view model.mainNavBarModel)
                , contentView
                ]

builderView : Model -> Element Msg
builderView model =
  let
    envSelectionView : Element Msg
    envSelectionView =
        none
--        map EnvSelectionMsg (EnvSelection.view (List.map .name model.environments))
  in
    row [ width fill ]
      [ el [ width fill ] (map BuilderAppMsg (BuilderApp.view model))
      , el [] envSelectionView
      ]

--postmanView : Element Msg
--postmanView =
--  Html.map PostmanMsg Postman.view
