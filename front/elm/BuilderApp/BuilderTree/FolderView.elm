module BuilderApp.BuilderTree.FolderView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.BuilderTree.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (Msg(..))

import Util.View as Util

folderReadView : Int -> String -> Bool -> Html Msg
folderReadView idx name open =
  let
    folderIcon = if open then "fas fa-folder-open fa-fw" else "fas fa-folder fa-fw"
  in
    a [ onClick (ToggleFolder idx) ]
      [ span [ class folderIcon ] []
      , text name
      ]

folderEditView : String -> Int -> Html Msg
folderEditView name idx =
  input [ value name, Util.onEnterWithInput (Rename idx) ] []

folderView : String -> Int -> List(Html Msg) -> Bool -> Bool -> Bool -> Html Msg
folderView name idx folderChildrenView open showMenu showRenameInput =
  let
    modeView =
      case showRenameInput of
        True -> folderEditView name idx
        False -> folderReadView idx name open
  in
    div []
      [ span []
        [ modeView
        , a [ onClick (ToggleMenu idx) ] [ span [ class "fas fa-ellipsis-h" ] []]
        , span [ hidden (not showMenu) ]
          [ a [ class "icono-tag", onClick (ShowRenameInput idx) ] [ text ("f2 ") ]
          , a [ class "icono-folder", onClick (Mkdir idx) ] [ text ("+/ ") ]
          , a [ class "icono-file", onClick (Touch idx) ] [ text ("+. ") ]
          , a [ class "icono-cross", onClick (Delete idx) ] [ ]
          ]
        ]
      , div [ class "columns" ]
        [ div [ class "column is-offset-1"]
          [ div [ hidden (not open) ] folderChildrenView ]
        ]
      ]
