module VarApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import VarApp.Message exposing (..)
import VarApp.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ id "varApp", class "columns" ]
    [ ul [ class "column" ] (List.indexedMap envView model)
    , defaultView
    ]

envView : Int -> (String, String) -> Html Msg
envView idx (varKey, varValue) =
    li []
        [ input [ id ""
                , placeholder "key"
                , onInput (PromptKey idx)
                , value varKey
                ] []
        , input [ id ""
                , placeholder "value"
                , onInput (PromptValue idx)
                , value varValue
                ] []
        , input [ onClick (DeleteInput idx), type_ "button" ]
            [  ]
        ]

defaultView : Html Msg
defaultView =
  div [ onClick AddNewInput, class "column" ]
      [ span [] [ text "add new var" ]
      ]
