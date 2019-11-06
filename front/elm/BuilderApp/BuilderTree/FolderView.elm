module BuilderApp.BuilderTree.FolderView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as Html
import Application.Type exposing (..)

import BuilderApp.BuilderTree.Message exposing (Msg(..))

import Util.View as Util

folderReadView : Int -> String -> Bool -> Element Msg
folderReadView idx name open =
    Input.button []
        { onPress = Just <| ToggleFolder idx
        , label = text name
        }

folderEditView : String -> Int -> Element Msg
folderEditView name idx =
  Input.text
      [ htmlAttribute <| Util.onEnterWithInput (Rename idx)
      ]
      { onChange = ChangeName idx
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename folder"
      }

folderView : Editable String -> Int -> List(Element Msg) -> Bool -> Bool -> Element Msg
folderView name idx folderChildrenView open showMenu =
    let
        modeView =
            case name of
                NotEdited value ->
                    folderReadView idx value open
                Edited oldValue newValue ->
                    folderEditView newValue idx
        menuView =
            row []
                [ Input.button []
                      { onPress = Just <| ShowRenameInput idx
                      , label = text "f2 "
                      }
                , Input.button []
                    { onPress = Just <| Mkdir idx
                    , label = text ("+/ ")
                    }
                , Input.button []
                    { onPress = Just <| Touch idx
                    , label = text ("+. ")
                    }
                , Input.button []
                    { onPress = Just <| Delete idx
                    , label = none
                    }
                ]
    in
        column [ Element.explain Debug.todo, padding 10 ]
            [ row []
                  [ modeView
                  , Input.button []
                      { onPress = Just <| ToggleMenu idx
                      , label = text " menu"
                      }
                  , case showMenu of
                        True -> menuView
                        False -> none
                  ]
            , column [] <|
                case open of
                    True -> folderChildrenView
                    False -> []
            ]
