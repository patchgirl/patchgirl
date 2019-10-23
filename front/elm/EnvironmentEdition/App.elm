module EnvironmentEdition.App exposing (..)

import List.Extra as List

import EnvironmentEdition.Message exposing (Msg(..))

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition
import EnvironmentEdition.Model exposing (..)
import EnvironmentEdition.Util exposing (..)
import Application.Type as Type

defaultEnvironment =
    { name = "new environment"
    , keyValues = []
    }

update : Msg -> Model a -> Model a
update msg model =
  case msg of
    SelectEnvToEdit idx ->
      { model | selectedEnvironmentToEditIndex = Just idx }

    Delete idx ->
      let
        newEnvironments = List.removeAt idx model.environments
        newSelectedEnvironmentToEditIndex =
          case model.selectedEnvironmentToEditIndex == Just idx of
            True -> Nothing
            False -> model.selectedEnvironmentToEditIndex
      in
          { model
              | selectedEnvironmentToEditIndex = newSelectedEnvironmentToEditIndex
              , environments = newEnvironments
          }

    Add ->
        let
            newEnvironments = model.environments ++ [ defaultEnvironment ]
        in
            { model | environments = newEnvironments }

    ShowRenameInput idx ->
        { model | selectedEnvironmentToRenameIndex = Just idx }

    Rename idx newEnvironmentName ->
        let
            updateEnv old = { old | name = newEnvironmentName }
            mNewEnvs = List.updateAt idx updateEnv model.environments
      in
          case mNewEnvs of
              newEnvs ->
                  { model
                      | selectedEnvironmentToRenameIndex = Nothing
                      , environments = newEnvs
                  }

    EnvironmentKeyValueEditionMsg idx subMsg ->
        case getEnvironmentToEdit model of
            Nothing ->
                model
            Just environment ->
                case EnvironmentKeyValueEdition.update subMsg environment of
                    newEnvironment ->
                        let
                            newEnvironments = List.setAt idx newEnvironment model.environments
                        in
                            { model | environments = newEnvironments }
