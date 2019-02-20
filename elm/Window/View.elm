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

import Util.List as List

import Tree.Model as Tree

import Window.Model exposing(..)
import Window.Message exposing(..)

view : Model -> Html Msg
view model =
  let
    contentView : Html Msg
    contentView =
      case model.tabModel of
        Tab.EnvTab -> div [] [ text "env" ]
        Tab.ReqTab -> div [] (builderView model)
{-        [ div [] [ postmanView ]
        , div [] [ treeView model ]
        , div [] [ envNavView model ]
        , div [] [ builderAppView model.treeModel ]
        , div [] [ envView model ]
        ]-}
  in
    div [ id "app" ]
        [ tabView
        , contentView
        ]

builderView : Model -> List(Html Msg)
builderView model =
  let
    treeView : Html Msg
    treeView =
      Html.map TreeMsg (Tree.view model.treeModel.tree)
    f idx = Tree.findBuilder model.treeModel.tree idx
        |> Maybe.map Builder.view
        |> Maybe.map (Html.map BuilderMsg)
    builderAppView : List (Html Msg)
    builderAppView =
      List.flatten (List.map f model.treeModel.displayedBuilderIndexes)
  in
    [ treeView
    , div [] builderAppView ]

tabView : Html Msg
tabView =
  Html.map TabMsg Tab.view

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view

envNavView : Model -> Html Msg
envNavView model =
  Html.map EnvNavMsg (EnvNav.view model.envNavModel)

envView : Model -> Html Msg
envView model =
  Html.map EnvMsg (Env.view model.envModel)
