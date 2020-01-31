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

folderMenuView : Int -> Bool -> Element Msg
folderMenuView idx isOpen =
    let
        iconClass =
            case isOpen of
                True -> "more_horiz"
                False -> "more_vert"
        menuIcon = icon iconClass
        menuView =
            row [ spacing 5 ]
                [ Input.button []
                      { onPress = Just <| ShowRenameInput idx
                      , label = editIcon
                      }
                , Input.button []
                    { onPress = Just <| RandomMkdir idx
                    , label = createFolderIcon
                    }
                , Input.button []
                    { onPress = Just <| RandomTouch idx
                    , label = createFileIcon
                    }
                , Input.button []
                    { onPress = Just <| Delete idx
                    , label = deleteIcon
                    }
                ]
    in
        case isOpen of
            True -> row [] [ menuIcon, menuView ]
            False -> row [] [ menuIcon ]


folderReadView : Int -> String -> Bool -> Element Msg
folderReadView idx name isOpen =
    Input.button []
        { onPress = Just <| ToggleFolder idx
        , label = folderWithIconView name isOpen
        }

folderEditView : Uuid.Uuid -> String -> Int -> Element Msg
folderEditView id name idx =
  Input.text
      [ htmlAttribute <| Util.onEnterWithInput (AskRename id idx)
      ]
      { onChange = ChangeName idx
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename folder"
      }

folderView : Uuid.Uuid -> Editable String -> Int -> List(Element Msg) -> Bool -> Bool -> Element Msg
folderView id name idx folderChildrenView open showMenu =
    let
        modeView =
            case name of
                NotEdited value ->
                    folderReadView idx value open
                Edited oldValue newValue ->
                    folderEditView id newValue idx
    in
        column [ width (fill |> maximum 300) ]
            [ row []
                  [ modeView
                  , Input.button []
                      { onPress = Just <| ToggleMenu idx
                      , label = folderMenuView idx showMenu
                      }
                  ]
            , case open of
                  True -> column [ spacing 10, paddingXY 20 10 ] folderChildrenView
                  False -> none
            ]
