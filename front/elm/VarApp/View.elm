module VarApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import VarApp.Message exposing (..)
import VarApp.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ id "varApp", class "columns" ]
    [ ul [ class "column is-full" ] (List.indexedMap envView model)
    ]

envView : Int -> (String, String) -> Html Msg
envView idx (varKey, varValue) =
    li []
        [ input [ id ""
                , placeholder "key"
                , value varKey
                ] []
        , input [ id ""
                , placeholder "value"
                , value varValue
                ] []
        ]
