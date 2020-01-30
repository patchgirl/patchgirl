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
import Element.Font as Font

import ViewUtil exposing (..)
import Util.View as Util

fileReadView : String -> Int -> Element Msg
fileReadView name idx =
    Input.button []
        { onPress = Just <| SetDisplayedBuilder idx
        , label = el [] <| iconWithTextAndColor "label" name secondaryColor
        }

fileEditView : String -> Int -> Int -> Element Msg
fileEditView name id idx =
  Input.text
      [ htmlAttribute <| Util.onEnterWithInput (AskRename id)
      ]
      { onChange = ChangeName idx
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename file"
      }

fileView : Int -> Editable String -> Int -> Bool -> Element Msg
fileView id name idx showMenu =
    let
        modeView =
            case name of
                NotEdited value -> fileReadView value idx
                Edited oldValue newValue -> fileEditView newValue id idx
        menuView =
            case not showMenu of
                True -> none
                False ->
                    row []
                        [ Input.button []
                              { onPress = Just <| ShowRenameInput idx
                              , label = editIcon
                              }
                        , Input.button []
                            { onPress = Just <| Delete idx
                            , label = deleteIcon
                            }
                        ]
  in
      row []
          [ modeView
          , Input.button []
              { onPress = Just <| ToggleMenu idx
              , label =
                  icon <|
                      case showMenu of
                          True -> "more_horiz"
                          False -> "more_vert"
              }
          , menuView
          ]
