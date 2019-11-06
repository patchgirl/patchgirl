module BuilderApp.BuilderTree.FileView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Application.Type exposing (..)

import BuilderApp.BuilderTree.Message exposing (Msg(..))

import Util.View as Util

fileReadView : String -> Int -> Html Msg
fileReadView name idx =
  a [ href "#", onClick (SetDisplayedBuilder idx) ]
    [ span [ class "fas fa-file fa-fw" ] []
    , text name
    ]

fileEditView : String -> Int -> Html Msg
fileEditView name idx =
  input [ value name, Util.onEnterWithInput (Rename idx) ] []

fileView : Editable String -> Int -> Bool -> Html Msg
fileView name idx showMenu =
  let
    modeView =
      case name of
        NotEdited value -> fileEditView value idx
        Edited oldValue newValue -> fileReadView newValue idx
  in
    div [ ]
      [ modeView
      , a [ onClick (ToggleMenu idx) ] [ span [ class "fas fa-ellipsis-h" ] []]
      , span [ hidden (not showMenu) ]
        [ a [ class "icono-tag", onClick (ShowRenameInput idx) ] []
        , a [ class "icono-cross", onClick (Delete idx) ] []
        ]
      ]
