module EnvironmentEdition.App exposing (..)

import List.Extra as List

import EnvironmentEdition.Message exposing (Msg(..))

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition
import EnvironmentEdition.Model exposing (..)
import EnvironmentEdition.Util exposing (..)
import Application.Type exposing (..)

defaultEnvironment =
    { id = 0
    , name = "new environment"
    , keyValues = NotEdited []
    }

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
  case msg of
    SelectEnvToEdit idx ->
      let
          newModel =
              { model | selectedEnvironmentToEditIndex = Just idx }
      in
          (newModel, Cmd.none)

    Delete idx ->
      let
        newEnvironments = List.removeAt idx model.environments
        newSelectedEnvironmentToEditIndex =
          case model.selectedEnvironmentToEditIndex == Just idx of
            True -> Nothing
            False -> model.selectedEnvironmentToEditIndex

        newModel =
            { model
                | selectedEnvironmentToEditIndex = newSelectedEnvironmentToEditIndex
                , environments = newEnvironments
             }
      in
          (newModel, Cmd.none)

    AskEnvironmentCreation name ->
        (model, Cmd.none)

    Add ->
        let
            newEnvironments =
                model.environments ++ [ defaultEnvironment ]

            newModel =
                { model | environments = newEnvironments }

        in
            (newModel, Cmd.none)

    ShowRenameInput idx ->
        let
            newModel =
                { model | selectedEnvironmentToRenameIndex = Just idx }
        in
            (newModel, Cmd.none)

    Rename idx newEnvironmentName ->
        let
            updateEnv old =
                { old | name = newEnvironmentName }

            mNewEnvs =
                List.updateAt idx updateEnv model.environments

            newModel =
                { model
                    | selectedEnvironmentToRenameIndex = Nothing
                    , environments = mNewEnvs
                }
      in
          (newModel, Cmd.none)


    ChangeName idx newEnvironmentName ->
        let
            updateEnv old =
                { old | name = newEnvironmentName }

            mNewEnvs =
                List.updateAt idx updateEnv model.environments

            newModel =
                { model
                    | environments = mNewEnvs
                }
      in
          (newModel, Cmd.none)


    EnvironmentKeyValueEditionMsg subMsg ->
        case getEnvironmentToEdit model of
            Nothing ->
                (model, Cmd.none)

            Just environment ->
                case EnvironmentKeyValueEdition.update subMsg environment of
                    newEnvironment ->
                        let
                            -- todo fix 0 -> should be model.selectedEnvironmentToEditIndex
                            newEnvironments = List.setAt 0 newEnvironment model.environments

                            newModel =
                                { model | environments = newEnvironments }

                        in
                            (newModel, Cmd.none)
