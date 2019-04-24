module Tree.FileView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))

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

fileView : String -> Int -> Bool -> Bool -> Html Msg
fileView name idx showMenu showRenameInput =
  let
    modeView =
      case showRenameInput of
        True -> fileEditView name idx
        False -> fileReadView name idx
  in
    div [ ]
      [ modeView
      , a [ onClick (ToggleMenu idx) ] [ span [] []]
      , span [ hidden (not showMenu) ]
        [ a [ class "icono-tag", onClick (ShowRenameInput idx) ] []
        , a [ class "icono-cross", onClick (Delete idx) ] []
        ]
      ]
