module EnvironmentKeyValueEdition.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvironmentKeyValueEdition.Message exposing (..)
import EnvironmentKeyValueEdition.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ id "envBuilder" ] ((List.indexedMap viewKeyValue model) ++ [defaultView])

viewKeyValue : Int -> (String, String) -> Html Msg
viewKeyValue idx (key, envValue) =
  div [ id "envForm" ]
    [ input [ placeholder "key", onInput (PromptKey idx), value key ] []
    , input [ placeholder "value", onInput (PromptValue idx), value envValue ] []
    , a [ href "#", class "icono-cross", onClick (DeleteKeyValue idx)] [ text "chia" ]
    ]

defaultView : Html Msg
defaultView =
  div [ onClick AddNewInput, class "centerHorizontal align-self-center" ]
      [ span [] [ text "+" ] ]
