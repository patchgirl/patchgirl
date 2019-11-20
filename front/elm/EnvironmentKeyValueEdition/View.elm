module EnvironmentKeyValueEdition.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events as Events
import ViewUtil exposing (..)
import EnvironmentKeyValueEdition.Message exposing (..)

type alias Model a =
    { a
        | keyValues : List(String, String)

    }

emptyModel = [("", "")]

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
