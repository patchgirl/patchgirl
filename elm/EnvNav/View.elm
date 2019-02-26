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
  div [ id "navEnv", class "flexRow" ]
    [ ul [ class "flexColumn" ] <| (List.indexedMap (entryView model.renameEnvIdx) model.envs) ++ [
           div [ onClick Add, class "centerHorizontal align-self-center" ] [ span [ class "icono-plusCircle" ] [] ]
          ]
    , ul [] <| List.indexedMap (envView model) model.envs
    ]

entryView : Maybe Int -> Int -> EnvInfo -> Html Msg
entryView renameEnvIdx idx envInfo =
  let
    readView = a [ href "#", onClick (Select idx) ] [ span [] [ text envInfo.name ] ]
    editView = input [ value envInfo.name, Util.onEnterWithInput (Rename idx) ] []
    modeView =
      case renameEnvIdx == Just idx of
        True -> editView
        False -> readView
  in
    li []
      [ modeView
      , a [ href "#", onClick (ShowRenameInput idx)] [ span [class "icono-hamburger"][] ]
      , a [ href "#", class "icono-cross", onClick (Delete idx)] [ text "-" ]
      ]

envView : Model -> Int -> EnvInfo -> Html Msg
envView model idx envInfo =
  case model.selectedEnvIndex == Just idx of
    showEnvTab ->
      div [ hidden (not showEnvTab) ] [ Html.map (EnvMsg idx) (Env.view envInfo.env) ]
