module Window.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.View as Tree
import Tree.Util as Tree
import Postman.View as Postman
import Env.View as Env
import EnvNav.View as EnvNav
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
          Tab.EnvTab -> [ text "env" ]
          Tab.ReqTab -> (builderView model)
{-        [ div [] [ postmanView ]
        , div [] [ treeView model ]
        , div [] [ envNavView model ]
        , div [] [ builderAppView model.treeModel ]
        , div [] [ envView model ]
        ]-}
  in
    div [ id "app" ]
        [ tabView model
        , contentView
        ]

builderView : Model -> List(Html Msg)
builderView model =
  let
    treeView : Html Msg
    treeView =
      Html.map TreeMsg (Tree.view model.treeModel.tree)
  in
    [ treeView
    , (Html.map BuildersMsg (Builders.view model.treeModel))
    ]

tabView : Model -> Html Msg
tabView model =
  Html.map TabMsg (Tab.view model.tabModel)

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view

envNavView : Model -> Html Msg
envNavView model =
  Html.map EnvNavMsg (EnvNav.view model.envNavModel)

envView : Model -> Html Msg
envView model =
  Html.map EnvMsg (Env.view model.envModel)
