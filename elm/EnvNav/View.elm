module EnvNav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvNav.Message exposing (..)
import EnvNav.Model exposing (..)
import Util.View as Util
import Env.View as Env

view : Model -> Html Msg
view model =
  div [ id "content" ]
    [ div [] <| (List.indexedMap (entryView model.renameEnvIdx) model.envs) ++ [ div [ onClick Add ] [ text "+" ] ]
    , ul [] <| List.indexedMap (envView model) model.envs
    ]

entryView : Maybe Int -> Int -> EnvInfo -> Html Msg
entryView renameEnvIdx idx envInfo =
  let
    readView = a [ href "#", onClick (Select idx) ] [ li [] [ text envInfo.name ] ]
    editView = input [ value envInfo.name, Util.onEnterWithInput (Rename idx) ] []
    modeView =
      case renameEnvIdx == Just idx of
        True -> editView
        False -> readView
  in
    div []
      [ modeView
      , a [ href "#", onClick (ShowRenameInput idx)] [ text "f2" ]
      , a [ href "#", onClick (Delete idx)] [ text "-" ]
      ]

envView : Model -> Int -> EnvInfo -> Html Msg
envView model idx envInfo =
  case model.selectedEnvIndex == Just idx of
    showEnvTab ->
      div [ hidden (not showEnvTab) ] [ Html.map (EnvMsg idx) (Env.view envInfo.env) ]
