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
  let
    modifiers = Bulma.tabsModifiers
    tabsModifiers = { modifiers | alignment = Bulma.Centered, size = Bulma.Medium }
  in
    Bulma.tabs tabsModifiers [] []
      [ tabView OpenReqWindow "Req" model
      , tabView OpenEnvWindow "Env" model
      ]

tabView : Msg -> String -> Model -> Html Msg
tabView msg str model =
  let
    isSelected = case (model, msg) of
      (EnvTab, OpenEnvWindow) -> True
      (ReqTab, OpenReqWindow) -> True
      _ -> False
  in
    Bulma.tab isSelected [ onClick msg ] [] [ text str ]
