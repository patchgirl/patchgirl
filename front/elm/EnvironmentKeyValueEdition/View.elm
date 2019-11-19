module EnvironmentKeyValueEdition.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events as Events
import ViewUtil exposing (..)
import EnvironmentKeyValueEdition.Message exposing (..)
import EnvironmentKeyValueEdition.Model exposing (..)

view : Model -> Element Msg
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
            [ column [ spacing 5 ] (List.indexedMap viewKeyValue model)
            , el [ centerX ] addNewKeyValueView
            ]


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
