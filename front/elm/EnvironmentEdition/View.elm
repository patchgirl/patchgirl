module EnvironmentEdition.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

import EnvironmentEdition.Message exposing (..)
import Util.View as Util
import ViewUtil exposing (..)
import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition
import EnvironmentEdition.Model exposing (..)
import Application.Type exposing (..)
import List.Extra as List

view : Model a -> Element Msg
view model =
    let
        mSelectedEnv : Maybe Environment
        mSelectedEnv =
            List.find (\env -> Just env.id == model.selectedEnvironmentToEditId) model.environments

        keyValuesEditionView =
            case mSelectedEnv of
                Just selectedEnv ->
                    el []
                        <| map EnvironmentKeyValueEditionMsg (EnvironmentKeyValueEdition.view selectedEnv)

                Nothing ->
                    el [] (text "no environment selected")

        envListView =
            (List.indexedMap (entryView model.selectedEnvironmentToRenameId model.selectedEnvironmentToEditId) model.environments)

        addEnvButtonView =
            Input.button []
                { onPress = Just <| (AskEnvironmentCreation "new environment")
                , label =
                    row []
                        [ addIcon
                        , el [] (text "Add environment")
                        ]
                }

    in
        row [ width fill
            , centerX
            , paddingXY 30 10
            ]
          [ column [ alignLeft, alignTop, spacing 10 ]
                [ column [ spacing 10 ] envListView
                , el [ centerX ] addEnvButtonView
                ]
          , el [ centerX, alignTop ] keyValuesEditionView
          ]


entryView : Maybe Int -> Maybe Int -> Int -> Environment -> Element Msg
entryView renameEnvId mSelectedEnvId idx environment =
  let
    readView =
        Input.button []
            { onPress = Just <| (SelectEnvToEdit idx)
            , label = el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue environment.name) secondaryColor
            }

    editView =
        Input.text [ htmlAttribute <| Util.onEnterWithInput (AskRename environment.id) ]
            { onChange = (ChangeName idx)
            , text = editedOrNotEditedValue environment.name
            , placeholder = Just <| Input.placeholder [] (text "environment name")
            , label = Input.labelHidden "rename environment"
            }

    modeView =
      case renameEnvId == Just environment.id of
        True -> editView
        False -> readView

    active =
        mSelectedEnvId == Just environment.id

  in
    row [ spacing 5 ]
      [ modeView
      , Input.button []
          { onPress = Just <| (ShowRenameInput environment.id)
          , label = editIcon
          }
      , Input.button []
          { onPress = Just <| (AskDelete environment.id)
          , label = el [] deleteIcon
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
