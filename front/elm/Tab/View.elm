module Tab.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Bulma.Components as Bulma
import Bulma.Modifiers as Bulma

import Tab.Model exposing(..)
import Tab.Message exposing(..)

view : Model -> Html Msg
view model =
  div [ id "mainNavBar" ] [
    ul []
      [ li [ onClick OpenReqWindow, class (tabClass2 model ReqTab) ] [ a [] [ text "Req" ] ]
      , li [ onClick OpenEnvWindow, class (tabClass2 model EnvTab) ] [ a [] [ text "Env" ] ]
      ]
  ]

tabClass2 : Model -> Model -> String
tabClass2 m1 m2 =
  case m1 == m2 of
    True -> "is-active"
    False -> ""

tabClass : Msg -> Model -> Model -> Html Msg
tabClass msg model1 model2 =
  let
    isSelected = case (model1, msg) of
      (EnvTab, OpenEnvWindow) -> True
      (ReqTab, OpenReqWindow) -> True
      _ -> False
  in
    Bulma.tab isSelected [ onClick msg ] [] [ ]
