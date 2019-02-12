module Window.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.View as Tree
import Postman.View as Postman
import Env.View as Env
import EnvNav.View as EnvNav
import Tab.View as Tab
import Builder.View as Builder

import Tree.Model as Tree

import Window.Model exposing(..)
import Window.Message exposing(..)

view : Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ tabView ]
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

builderAppView : Tree.Model -> Html Msg
builderAppView treeModel =
  div [][]
--  treeModel.displayedBuilderIndex
--    |> Maybe.andThen (Tree.findBuilder treeModel.tree)
---    |> Maybe.map Builder.view
--    |> Maybe.map (Html.map BuilderMsg)
--    |> Maybe.withDefault (div [] [ text (Maybe.withDefault "nope" (Maybe.map String.fromInt(treeModel.displayedBuilderIndex))) ])

treeView : Model -> Html Msg
treeView model =
  Html.map TreeMsg (Tree.view model.treeModel.tree)

envNavView : Model -> Html Msg
envNavView model =
  Html.map EnvNavMsg (EnvNav.view model.envNavModel)

envView : Model -> Html Msg
envView model =
  Html.map EnvMsg (Env.view model.envModel)
