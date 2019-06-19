module MainNavBar.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Bulma.Components as Bulma
import Bulma.Modifiers as Bulma

import MainNavBar.Model exposing(..)
import MainNavBar.Message exposing(..)

view : Model -> Html Msg
view model =
    let
        isActive : Model -> Model -> String
        isActive m1 m2 =
            case m1 == m2 of
                True -> "is-active"
                False -> ""
    in
        div [ id "mainNavBar" ] [
             ul []
                 [ li [ onClick OpenReqTab, class (isActive model ReqTab) ] [ a [] [ text "Req" ] ]
                 , li [ onClick OpenEnvTab, class (isActive model EnvTab) ] [ a [] [ text "Env" ] ]
                 , li [ onClick OpenVarTab, class (isActive model VarTab) ] [ a [] [ text "Var" ] ]
                 , li [ onClick OpenWorkspaceTab, class (isActive model WorkspaceTab) ] [ a [] [ text "Workspace" ] ]
                 ]
            ]
