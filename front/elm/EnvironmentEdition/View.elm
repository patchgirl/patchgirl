module EnvironmentEdition.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

import EnvironmentEdition.Message exposing (..)
import Util.View as Util
import ViewUtil exposing (..)
import EnvironmentKeyValueEdition.View as EnvironmentKeyValueEdition
import EnvironmentEdition.Model exposing (..)
import Application.Type as Type
import List.Extra as List

view : Model a -> Element Msg
view model =
    let
        mSelectedEnv : Maybe Type.Environment
        mSelectedEnv =
            Maybe.andThen (\idx -> List.getAt idx model.environments) model.selectedEnvironmentToEditIndex

        keyValuesEditionView =
            case mSelectedEnv of
                Just selectedEnv ->
                    el [ centerX ]
                        <| map EnvironmentKeyValueEditionMsg (EnvironmentKeyValueEdition.view (Debug.log "coucouo" selectedEnv.keyValues))

                Nothing ->
                    el [] (text "no environment selected")

        envListView =
            (List.indexedMap (entryView model.selectedEnvironmentToRenameIndex model.selectedEnvironmentToEditIndex) model.environments)

        addEnvButtonView =
            Input.button []
                { onPress = Just <| (Add)
                , label =
                    row [ spacing 5 ]
                        [ addIcon
                        , el [] (text "new environment")
                        ]
                }

    in
        row [ width fill ]
          [ column [alignLeft, paddingXY 10 0, alignTop, spacing 10]
                [ column [ spacing 10 ] envListView
                , el [centerX] addEnvButtonView
                ]
          , keyValuesEditionView
          ]


entryView : Maybe Int -> Maybe Int -> Int -> Type.Environment -> Element Msg
entryView renameEnvIdx mSelectedEnvIdx idx environment =
  let
    readView =
        Input.button []
            { onPress = Just <| (SelectEnvToEdit idx)
            , label = el [] <| iconWithTextAndColor "label" environment.name secondaryColor
            }

    editView =
        Input.text []
            { onChange = (Rename idx)
            , text = environment.name
            , placeholder = Just <| Input.placeholder [] (text environment.name)
            , label = labelInputView "environment name: "
            }

    modeView =
      case renameEnvIdx == Just idx of
        True -> editView
        False -> readView

    active =
        mSelectedEnvIdx == Just idx

  in
    row [ {-class active-} ]
      [ modeView
      , Input.button []
          { onPress = Just <| (ShowRenameInput idx)
          , label = el [] (text "Rename")
          }
      , Input.button []
          { onPress = Just <| (Delete idx)
          , label = el [] (text "Delete")
          }
      ]

{-
view : Model a -> Html Msg
view model =
    let
        foo =
            ul [ class "column" ] <|
              List.indexedMap (envView model) model.environments
        bar = (List.indexedMap (entryView model.selectedEnvironmentToRenameIndex model.selectedEnvironmentToEditIndex) model.environments)

        baz = div [ onClick Add, class "centerHorizontal align-self-center" ] [ text "+" ]
    in
        div [ id "envApp", class "columns" ]
          [ ul [ class "column is-offset-1 is-1" ] (bar ++ [ baz ])
          , foo
          ]

entryView : Maybe Int -> Maybe Int -> Int -> Type.Environment -> Html Msg
entryView renameEnvIdx mSelectedEnvIdx idx environment =
  let
    readView = a [ href "#", onClick (SelectEnvToEdit idx) ] [ span [] [ text environment.name ] ]
    editView = input [ value environment.name, Util.onEnterWithInput (Rename idx) ] []
    modeView =
      case renameEnvIdx == Just idx of
        True -> editView
        False -> readView
    active =
        case mSelectedEnvIdx == Just idx of
            True -> "active"
            False -> ""
  in
    li [ class active ]
      [ modeView
      , a [ href "#", onClick (ShowRenameInput idx)] [ text " Rename" ]
      , a [ href "#", onClick (Delete idx)] [ text " -" ]
      ]

envView : Model a -> Int -> Type.Environment -> Html Msg
envView model idx environment =
    let
        isEnvSelected = model.selectedEnvironmentToEditIndex == Just idx
    in
        div [ hidden (not isEnvSelected) ]
            [ Html.map (EnvironmentKeyValueEditionMsg idx) (EnvironmentKeyValueEdition.view environment.keyValues) ]
-}

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
