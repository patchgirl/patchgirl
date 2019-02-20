module Tab.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tab.Model exposing(..)
import Tab.Message exposing(..)

view : Model -> Html Msg
view model =
  div [ id "tab" ]
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
    selectedClass = case isSelected of
      False -> "unselected"
      True -> "selected"
  in
    div [ class ("tab " ++ selectedClass), onClick msg ] [ text str ]
