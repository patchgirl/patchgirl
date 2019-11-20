module EnvironmentKeyValueEdition.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events as Events
import ViewUtil exposing (..)

import Util.KeyValue.Util as KeyValue
import Application.Type as Type

-- * Model

type alias Model a =
    { a
        | keyValues : List(String, String)
    }

emptyModel = [("", "")]

-- * Message

type Msg
  = PromptKey Int String
  | PromptValue Int String
  | AddNewInput
  | DeleteKeyValue Int

-- * Update

update : Msg -> Type.Environment -> Type.Environment
update msg model =
    case msg of
        PromptKey idx str ->
            let
                newKeyValues = KeyValue.modify (KeyValue.changeKey str) idx model.keyValues
            in
                { model | keyValues = newKeyValues }

        PromptValue idx str ->
            let
                newKeyValues = KeyValue.modify (KeyValue.changeValue str) idx model.keyValues
            in
                { model | keyValues = newKeyValues }

        AddNewInput ->
            let
                newKeyValues = model.keyValues ++ emptyModel
            in
                { model | keyValues = newKeyValues }

        DeleteKeyValue idx ->
            let
                newKeyValues = KeyValue.delete idx model.keyValues
            in
                { model | keyValues = Debug.log "tai" newKeyValues }

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
    none
{-
    let
        name = notEditedValue model.name
    in
        row [ centerX, paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" (name) secondaryColor
            , mainActionButtonsView
            ]
-}

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
