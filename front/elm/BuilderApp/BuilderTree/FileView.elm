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
import Uuid

fileReadView : String -> Uuid.Uuid -> Element Msg
fileReadView name id =
    Input.button []
        { onPress = Just <| SetDisplayedBuilder id
        , label = el [] <| iconWithTextAndColor "label" name secondaryColor
        }

fileEditView : String -> Uuid.Uuid -> Element Msg
fileEditView name id =
  Input.text
      [ htmlAttribute <| Util.onEnterWithInput (AskRename id)
      ]
      { onChange = ChangeName id
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename file"
      }

fileView : Uuid.Uuid -> Maybe Uuid.Uuid -> Editable String -> Element Msg
fileView id mDisplayedRequestNodeMenuIndex name =
    let
        modeView =
            case name of
                NotEdited value -> fileReadView value id
                Edited oldValue newValue -> fileEditView newValue id

        showMenu =
            mDisplayedRequestNodeMenuIndex == Just id

        menuView =
            case not showMenu of
                True -> none
                False ->
                    row []
                        [ Input.button []
                              { onPress = Just <| ShowRenameInput id
                              , label = editIcon
                              }
                        , Input.button []
                            { onPress = Just <| AskDelete id
                            , label = deleteIcon
                            }
                        ]
  in
      row []
          [ modeView
          , Input.button []
              { onPress = Just <| ToggleMenu id
              , label =
                  icon <|
                      case showMenu of
                          True -> "more_horiz"
                          False -> "more_vert"
              }
          , menuView
          ]
