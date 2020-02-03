module BuilderApp.BuilderTree.FolderView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as Html
import Html as Html
import Application.Type exposing (..)
import ViewUtil exposing (..)
import Uuid

import BuilderApp.BuilderTree.Message exposing (Msg(..))

import Util.View as Util

folderWithIconView : String -> Bool -> Element Msg
folderWithIconView name isOpen =
    let
        folderIconText =
            case isOpen of
                False -> "keyboard_arrow_right"
                True -> "keyboard_arrow_down"
    in
        iconWithText folderIconText name

folderMenuView : Uuid.Uuid -> Bool -> Element Msg
folderMenuView id isOpen =
    let
        iconClass =
            case isOpen of
                True -> "more_horiz"
                False -> "more_vert"
        menuIcon = icon iconClass
        menuView =
            row [ spacing 5 ]
                [ Input.button []
                      { onPress = Just <| ShowRenameInput id
                      , label = editIcon
                      }
                , Input.button []
                    { onPress = Just <| GenerateRandomUUIDForFolder id
                    , label = createFolderIcon
                    }
                , Input.button []
                    { onPress = Just <| GenerateRandomUUIDForFile id
                    , label = createFileIcon
                    }
                , Input.button []
                    { onPress = Just <| AskDelete id
                    , label = deleteIcon
                    }
                ]
    in
        case isOpen of
            True -> row [] [ menuIcon, menuView ]
            False -> row [] [ menuIcon ]


folderReadView : Uuid.Uuid -> String -> Bool -> Element Msg
folderReadView id name isOpen =
    Input.button []
        { onPress = Just <| ToggleFolder id
        , label = folderWithIconView name isOpen
        }

folderEditView : Uuid.Uuid -> String -> Element Msg
folderEditView id name =
  Input.text
      [ htmlAttribute <| Util.onEnterWithInput (AskRename id)
      ]
      { onChange = ChangeName id
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename folder"
      }

folderView : Uuid.Uuid -> Maybe Uuid.Uuid -> Editable String -> List(Element Msg) -> Bool -> Element Msg
folderView id mDisplayedRequestNodeMenuIndex name folderChildrenView open =
    let
        modeView =
            case name of
                NotEdited value ->
                    folderReadView id value open
                Edited oldValue newValue ->
                    folderEditView id newValue

        showMenu =
            Just id == mDisplayedRequestNodeMenuIndex
    in
        column [ width (fill |> maximum 300) ]
            [ row []
                  [ modeView
                  , Input.button []
                      { onPress = Just <| ToggleMenu id
                      , label = folderMenuView id showMenu
                      }
                  ]
            , case open of
                  True -> column [ spacing 10, paddingXY 20 10 ] folderChildrenView
                  False -> none
            ]
