module Tab.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tab.Model exposing(..)
import Tab.Message exposing(..)
import Tab.Message exposing(..)

view : Html Msg
view =
  ul []
    [ li [ onClick OpenReqWindow ] [ text "Req" ]
    , li [ onClick OpenEnvWindow ] [ text "Env" ]
    ]
