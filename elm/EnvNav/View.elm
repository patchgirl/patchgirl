module EnvNav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvNav.Message exposing (..)
import EnvNav.Model exposing (..)

import Env.View as Env

view : Model -> Html Msg
view model =
  div [ id "content" ]
    [ (ul [] <| List.indexedMap entryView model.envs)
    , ul [] <| List.indexedMap (envView model) model.envs
    ]

entryView : Int -> EnvInfo -> Html Msg
entryView idx envInfo =
  div []
    [ a [ href "#", onClick (Select idx) ] [ li [] [ text envInfo.name ] ]
    , a [ href "#", onClick (Delete idx)] [ text "-" ]
    ]

envView : Model -> Int -> EnvInfo -> Html Msg
envView model idx envInfo =
  case model.selectedEnvIndex == Just idx of
    showEnvTab ->
      div [ hidden (not showEnvTab) ] [ Html.map (EnvMsg idx) (Env.view envInfo.env) ]
