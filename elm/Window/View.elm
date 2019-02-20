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

import Tree.Model as Tree

import Window.Model exposing(..)
import Window.Message exposing(..)

view : Model -> Html Msg
view model =
  let
    builderAppView : Html Msg
    builderAppView =
      model.treeModel.displayedBuilderIndex
        |> Maybe.andThen (Tree.findBuilder model.treeModel.tree)
        |> Maybe.map Builder.view
        |> Maybe.map (Html.map BuilderMsg)
        |> Maybe.withDefault (div [] [ text (Maybe.withDefault "nope" (Maybe.map String.fromInt(model.treeModel.displayedBuilderIndex))) ])
    treeView : Html Msg
    treeView =
      Html.map TreeMsg (Tree.view model.treeModel.tree)
    contentView : Html Msg
    contentView =
      case model.tabModel of
        Tab.EnvTab -> div [] [ text "env" ]
        Tab.ReqTab ->
          div []
            [ treeView
            , builderAppView
            ]
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ tabView ]
        , contentView
        ]
{-        [ div [] [ postmanView ]
        , div [] [ treeView model ]
        , div [] [ envNavView model ]
        , div [] [ builderAppView model.treeModel ]
        , div [] [ envView model ]
        ]-}
  in
    div [ id "app" ] [ builderView ]

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
