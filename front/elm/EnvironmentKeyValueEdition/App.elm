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
import List.Extra as List

-- * Model

type alias Model a =
    { a
        | keyValues : List (Storable Type.NewKeyValue Type.KeyValue)
        , name : Editable String
    }

newDefaultKeyValue : Storable Type.NewKeyValue Type.KeyValue
newDefaultKeyValue =
    New { key = "", value = "" }

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
--                newKeyValues =
--                    KeyValue.modify2 (KeyValue.changeKey str) idx (editedOrNotEditedValue model.keyValues)

                newKeyValues =
                    List.updateAt idx (\sKeyValue ->
                                           case sKeyValue of
                                               New new ->
                                                 New { new | key = str }

                                               Saved saved ->
                                                   Edited2 saved { saved | key = str }

                                               Edited2 saved edited ->
                                                   Edited2 saved { edited | key = str }
                                      ) model.keyValues

--                newEditedKeyValues =
--                    changeEditedValue2 model.keyValues (Edited (notEditedValue model.keyValues) newKeyValues)
            in
                { model | keyValues = newKeyValues }

        PromptValue idx str ->
            let
                newKeyValues =
                    List.updateAt idx (\sKeyValue ->
                                           case sKeyValue of
                                               New new ->
                                                 New { new | value = str }

                                               Saved saved ->
                                                   Edited2 saved { saved | value = str }

                                               Edited2 saved edited ->
                                                   Edited2 saved { edited | value = str }
                                      ) model.keyValues
            in
                { model | keyValues = newKeyValues }

        AddNewInput ->
            let
                newKeyValues =
                    model.keyValues ++ [ newDefaultKeyValue ]
            in
                { model | keyValues = newKeyValues }

        DeleteKeyValue idx ->
            let
                newKeyValues =
                    List.removeAt idx model.keyValues
            in
                { model | keyValues = newKeyValues }

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
            , column [ spacing 5 ] (List.indexedMap viewKeyValue model.keyValues)
            , el [ centerX ] addNewKeyValueView
            ]

titleView : Model a -> Element Msg
titleView model =
    let
        isModelDirty =
            List.any isDirty2 model.keyValues

        name =
            case isModelDirty of
                True ->
                    (editedOrNotEditedValue model.name) ++ "*"

                False ->
                    editedOrNotEditedValue model.name
    in
        row [ centerX, paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" name secondaryColor
            , case isModelDirty of
                  True -> mainActionButtonsView
                  False -> none
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

viewKeyValue : Int -> Storable NewKeyValue KeyValue -> Element Msg
viewKeyValue idx sKeyValue =
  row [ spacing 5 ]
      [ Input.text []
            { onChange = (PromptKey idx)
            , text =
                case sKeyValue of
                    New { key } ->
                        key

                    Saved { key } ->
                        key

                    Edited2 _ { key } ->
                        key

            , placeholder = Just <| Input.placeholder [] (text "key")
            , label = Input.labelHidden "Key: "
            }
      , Input.text []
            { onChange = (PromptValue idx)
            , text =
                case sKeyValue of
                    New { value } ->
                        value

                    Saved { value } ->
                        value

                    Edited2 _ { value } ->
                        value
            , placeholder = Just <| Input.placeholder [] (text "value")
            , label = Input.labelHidden "Value: "
            }
      , Input.button []
          { onPress = Just <| (DeleteKeyValue idx)
          , label = el [] deleteIcon
          }
      ]
