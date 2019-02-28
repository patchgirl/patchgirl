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
          Tab.EnvTab -> [ envsView model ]
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

envsView : Model -> Html Msg
envsView model =
  div [] [ envNavView model
         ]

envNavView : Model -> Html Msg
envNavView model =
  Html.map EnvNavMsg (EnvNav.view model.envNavModel)

builderView : Model -> List(Html Msg)
builderView model =
  let
    treeView : Html Msg
    treeView =
      Html.map TreeMsg (Tree.view model.treeModel.tree)
    envSelectionView : Html Msg
    envSelectionView =
      Html.map EnvSelectionMsg (EnvSelection.view model.selectedEnvModel)
  in
    [ treeView
    , (Html.map BuildersMsg (Builders.view model.treeModel))
    , envSelectionView
    ]

tabView : Model -> Html Msg
tabView model =
  Html.map TabMsg (Tab.view model.tabModel)

postmanView : Html Msg
postmanView =
  Html.map PostmanMsg Postman.view
