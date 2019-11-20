module EnvironmentKeyValueEdition.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events as Events
import ViewUtil exposing (..)
import Application.Type exposing(..)
import Util.KeyValue.Util as KeyValue
import Application.Type as Type

-- * Model

type alias Model a =
    { a
        | keyValues : Editable (List(String, String))
        , name : String
    }

-- * Message

type Msg
  = PromptKey Int String
  | PromptValue Int String
  | AddNewInput
  | AskSave
  | DeleteKeyValue Int

-- * Update

update : Msg -> Type.Environment -> Type.Environment
update msg model =
    case msg of
        PromptKey idx str ->
            let
                newKeyValues =
                    KeyValue.modify (KeyValue.changeKey str) idx (editedOrNotEditedValue model.keyValues)

                newEditedKeyValues =
                    changeEditedValue2 model.keyValues (Edited (notEditedValue model.keyValues) newKeyValues)
            in
                { model | keyValues = newEditedKeyValues }

        PromptValue idx str ->
            let
                newKeyValues =
                    KeyValue.modify (KeyValue.changeValue str) idx (editedOrNotEditedValue model.keyValues)

                newEditedKeyValues =
                    changeEditedValue2 model.keyValues (Edited (notEditedValue model.keyValues) newKeyValues)
            in
                { model | keyValues = newEditedKeyValues }

        AddNewInput ->
            let
                newKeyValues =
                    changeEditedValue ((editedOrNotEditedValue model.keyValues) ++ [("", "")]) model.keyValues

            in
                { model | keyValues = newKeyValues }

        DeleteKeyValue idx ->
            let
                newKeyValues =
                    KeyValue.delete idx (editedOrNotEditedValue model.keyValues)

                newEditedKeyValues =
                    changeEditedValue2 model.keyValues (Edited (notEditedValue model.keyValues) newKeyValues)

            in
                { model | keyValues = newEditedKeyValues }

        AskSave ->
            model

-- * View

view : Model a -> Element Msg
view model =
    let
        addNewKeyValueView =
            Input.button []
                { onPress = Just <| AddNewInput
                , label =
                    row []
                        [ addIcon
                        , el [] (text "Add key value")
                        ]
                }
    in
        column [ spacing 10 ]
            [ titleView model
            , column [ spacing 5 ] (List.indexedMap viewKeyValue (editedOrNotEditedValue model.keyValues))
            , el [ centerX ] addNewKeyValueView
            ]

titleView : Model a -> Element Msg
titleView model =
    let
        name =
            case isDirty (Debug.log "teto" model.keyValues) of
                True ->
                    model.name ++ "*"

                False ->
                    model.name
    in
        row [ centerX, paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" name secondaryColor
            , mainActionButtonsView
            ]

mainActionButtonsView : Element Msg
mainActionButtonsView =
    let
        inputParam =
            [ Border.solid
            , Border.color secondaryColor
            , Border.width 1
            , Border.rounded 5
            , alignBottom
            , Background.color secondaryColor
            , paddingXY 10 10
            ]
    in
        Input.button inputParam
            { onPress = Just <| AskSave
            , label = el [ centerY] <| iconWithTextAndColor "save" "Save" primaryColor
            }

viewKeyValue : Int -> (String, String) -> Element Msg
viewKeyValue idx (key, envValue) =
  row [ spacing 5 ]
      [ Input.text []
            { onChange = (PromptKey idx)
            , text = key
            , placeholder = Just <| Input.placeholder [] (text "key")
            , label = Input.labelHidden "Key: "
            }
      , Input.text []
            { onChange = (PromptValue idx)
            , text = envValue
            , placeholder = Just <| Input.placeholder [] (text "value")
            , label = Input.labelHidden "Value: "
            }
      , Input.button []
          { onPress = Just <| (DeleteKeyValue idx)
          , label = el [] deleteIcon
          }
      ]
