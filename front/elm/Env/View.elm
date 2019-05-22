module Env.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Env.Message exposing (..)
import Env.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ class "foo" ] ((List.indexedMap viewKeyValue model) ++ [defaultView])

viewKeyValue : Int -> (String, String) -> Html Msg
viewKeyValue idx (key, envValue) =
  div [ ]
    [ input [ placeholder "key", onInput (PromptKey idx), value key ] []
    , input [ placeholder "value", onInput (PromptValue idx), value envValue ] []
    , a [ href "#", class "icono-cross", onClick (DeleteInput idx)] [ text "-" ]
    ]

defaultView : Html Msg
defaultView =
  div [ onClick AddNewInput, class "centerHorizontal align-self-center" ] [ span [ class "icono-plusCircle" ] [ text "test" ] ]
