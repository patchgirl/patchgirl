module Window.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.View as Tree
import Tree.Util as Tree
import Postman.View as Postman
import Env.View as Env
import EnvNav.View as EnvNav
import EnvSelection.View as EnvSelection
import Tab.View as Tab
import Tab.Model as Tab
import Builder.View as Builder

import Builders.View as Builders

import Util.List as List

import Tree.Model as Tree

import Window.Model exposing(..)
import Window.Message exposing(..)

view : Model -> Html Msg
view model =
  let
    contentView : Html Msg
    contentView =
      div [ id "content" ] <|
        case model.tabModel of
          Tab.EnvTab -> [ envView model ]
          Tab.ReqTab -> [ builderView model ]
  in
    div [] [ tabView model, contentView ]
{-        [ div [] [ postmanView ]
        , div [] [ treeView model ]
        , div [] [ envNavView model ]
        , div [] [ builderAppView model.treeModel ]
        , div [] [ envView model ]
        ]-}

envView : Model -> Html Msg
envView model =
  Html.map EnvNavMsg (EnvNav.view model.envNavModel)

builderView : Model -> Html Msg
builderView model =
  let
    treeView : Html Msg
    treeView =
      Html.map TreeMsg (Tree.view model.treeModel)
    envSelectionView : Html Msg
    envSelectionView =
      Html.map EnvSelectionMsg (EnvSelection.view model.selectedEnvModel)
  in
    div [ id "builderApp" ]
      [ div [ id "treeView" ] [ treeView ]
      , div [ id "builderView" ] [ (Html.map BuildersMsg (Builders.view model.treeModel)) ]
      , div [ class "" ] [ envSelectionView ]
      ]

tabView : Model -> Html Msg
tabView model =
  Html.map TabMsg (Tab.view model.tabModel)

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view
