module Env.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Env.Message exposing (..)
import Env.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ ] (List.indexedMap viewKeyValue model)

viewKeyValue : Int -> (String, String) -> Html Msg
viewKeyValue idx (key, envValue) =
  div []
    [ input [ placeholder "key", onInput (PromptKey idx), value key ] []
    , input [ placeholder "value", onInput (PromptValue idx), value envValue ] []
    , button [ onClick (AddNewInput idx) ] [ text "new" ]
    , button [ onClick (DeleteInput idx) ] [ text "delete" ]
    ]
