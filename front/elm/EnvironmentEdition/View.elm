module EnvironmentEdition.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvironmentEdition.Message exposing (..)
import Util.View as Util
import EnvironmentKeyValueEdition.View as EnvironmentKeyValueEdition
import Window.Type as Type

type alias Model a =
    { a
        | environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditIndex : Maybe Int
        , selectedEnvironmentToRenameIndex : Maybe Int
    }

view : Model a -> Html Msg
view model =
    let
        foo =
            ul [ class "column" ] <|
              List.indexedMap (envView model) model.environments
        bar = (List.indexedMap (entryView model.selectedEnvironmentToRenameIndex model.selectedEnvironmentToEditIndex) model.environments)
        baz = div [ onClick Add, class "centerHorizontal align-self-center" ] [ text "+" ]
    in
        div [ id "envApp", class "columns" ]
          [ ul [ class "column is-offset-1 is-1" ] (bar ++ [ baz ])
          , foo
          ]

entryView : Maybe Int -> Maybe Int -> Int -> Type.Environment -> Html Msg
entryView renameEnvIdx mSelectedEnvIdx idx environment =
  let
    readView = a [ href "#", onClick (SelectEnvToEdit idx) ] [ span [] [ text environment.name ] ]
    editView = input [ value environment.name, Util.onEnterWithInput (Rename idx) ] []
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
      , a [ href "#", onClick (ShowRenameInput idx)] [ text " Rename" ]
      , a [ href "#", onClick (Delete idx)] [ text " -" ]
      ]

envView : Model a -> Int -> Type.Environment -> Html Msg
envView model idx environment =
    let
        isEnvSelected = model.selectedEnvironmentToEditIndex == Just idx
    in
        div [ hidden (not isEnvSelected) ]
            [ Html.map (EnvironmentKeyValueEditionMsg idx) (EnvironmentKeyValueEdition.view environment.keyValues) ]
