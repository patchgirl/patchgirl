module EnvToRun.EnvNav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvToRun.EnvNav.Message exposing (..)
import Util.View as Util
import EnvToRun.View as EnvToRun

type alias Environment =
    { environmentName : String
    , keyValues : List(String, String)
    }

type alias Model a =
    { a
        | environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditIndex : Maybe Int
        , selectedEnvironmentToRenameIndex : Maybe Int
    }

view : Model a -> Html Msg
view model =
  div [ id "envApp", class "columns" ]
    [ ul [ class "column is-offset-1 is-1" ] <|
          (List.indexedMap (entryView model.selectedEnvironmentToRenameIndex model.selectedEnvironmentToEditIndex) model.environments) ++ [
               div [ onClick Add, class "centerHorizontal align-self-center" ] [ span [ class "icono-plusCircle" ] [] ]
          ]
    , ul [ class "column" ] <| List.indexedMap (envView model) model.environments
    ]

entryView : Maybe Int -> Maybe Int -> Int -> Environment -> Html Msg
entryView renameEnvIdx mSelectedEnvIdx idx environment =
  let
    readView = a [ href "#", onClick (SelectEnvToEdit idx) ] [ span [] [ text environment.environmentName ] ]
    editView = input [ value environment.environmentName, Util.onEnterWithInput (Rename idx) ] []
    modeView =
      case renameEnvIdx == Just idx of
        True -> editView
        False -> readView
    active =
        case mSelectedEnvIdx == Just idx of
            True -> "active"
            False -> ""
  in
    li [ class active ]
      [ modeView
      , a [ href "#", onClick (ShowRenameInput idx)] [ span [class "icono-hamburger"][] ]
      , a [ href "#", class "icono-cross", onClick (Delete idx)] [ text "-" ]
      ]

envView : Model a -> Int -> Environment -> Html Msg
envView model idx environment =
  case model.selectedEnvironmentToEditIndex == Just idx of
    showEnvTab ->
      div [ hidden (not showEnvTab) ] [ Html.map (EnvToRunMsg idx) (EnvToRun.view environment.keyValues) ]
