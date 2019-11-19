module EnvironmentKeyValueEdition.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events as Events

import EnvironmentKeyValueEdition.Message exposing (..)
import EnvironmentKeyValueEdition.Model exposing (..)

view : Model -> Element Msg
view model =
    column []
        <| (List.indexedMap viewKeyValue model) ++ [defaultView]

defaultView : Element Msg
defaultView =
    Input.button []
        { onPress = Just <| AddNewInput
        , label = el [] (text "+")
        }

viewKeyValue : Int -> (String, String) -> Element Msg
viewKeyValue idx (key, envValue) =
  row []
      [ Input.text []
            { onChange = (PromptKey idx)
            , text = key
            , placeholder = Just <| Input.placeholder [] (text "key")
            , label = labelInputView "Key: "
            }
      , Input.text []
            { onChange = (PromptValue idx)
            , text = envValue
            , placeholder = Just <| Input.placeholder [] (text "value")
            , label = labelInputView "Value: "
            }
      , Input.button []
          { onPress = Just <| (DeleteKeyValue idx)
          , label = el [] (text "+")
          }
      ]

labelInputView : String -> Input.Label Msg
labelInputView labelText =
    let
        size =
            width (fill
                  |> maximum 100
                  |> minimum 100
                  )
    in
        Input.labelAbove [ centerY, size ] <| text labelText


{-
view : Model -> Html Msg
view model =
  div [ id "envBuilder" ] ((List.indexedMap viewKeyValue model) ++ [defaultView])

viewKeyValue : Int -> (String, String) -> Html Msg
viewKeyValue idx (key, envValue) =
  div [ id "envForm" ]
    [ input [ placeholder "key", onInput (PromptKey idx), value key ] []
    , input [ placeholder "value", onInput (PromptValue idx), value envValue ] []
    , a [ href "#", class "icono-cross", onClick (DeleteKeyValue idx)] [ text "chia" ]
    ]

defaultView : Html Msg
defaultView =
  div [ onClick AddNewInput, class "centerHorizontal align-self-center" ]
      [ span [] [ text "+" ] ]
-}
