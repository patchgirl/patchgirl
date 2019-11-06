module BuilderApp.BuilderTree.FileView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as Html
import Html as Html

import Application.Type exposing (..)

import BuilderApp.BuilderTree.Message exposing (Msg(..))
import Icon exposing (..)
import Element.Font as Font

import Color exposing (..)
import Util.View as Util

fileReadView : String -> Int -> Element Msg
fileReadView name idx =
    el [] <| iconWithTextAndColor "label" name secondaryColor
{-
  Html.a [ Html.href "#", Html.onClick (SetDisplayedBuilder idx) ]
    [ Html.span [ Html.class "fas fa-file fa-fw" ] []
    , Html.text name
    ]
    -}

fileEditView : String -> Int -> Element Msg
fileEditView name idx =
    el [] <| text ""
--  input [ value name, Util.onEnterWithInput (Rename idx) ] []

fileView : Editable String -> Int -> Bool -> Element Msg
fileView name idx showMenu =
    let
        modeView =
            case name of
                NotEdited value -> fileReadView value idx
                Edited oldValue newValue -> fileEditView newValue idx
        menuView =
            case not showMenu of
                True -> none
                False ->
                    row []
                        [ Input.button []
                              { onPress = Just <| ShowRenameInput idx
                              , label = text "showRename"
                              }
                        , Input.button []
                            { onPress = Just <| Delete idx
                            , label = text "delete"
                            }
                        ]
  in
      row []
          [ modeView
          , Input.button []
              { onPress = Just <| ToggleMenu idx
              , label = text ""
              }
          , menuView
          ]
