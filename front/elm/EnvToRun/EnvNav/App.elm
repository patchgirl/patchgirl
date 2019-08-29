module EnvToRun.EnvNav.App exposing (..)

import List.Extra as List

import EnvToRun.EnvNav.Message exposing (Msg(..))

import EnvToRun.App as EnvToRun
import Window.Type as Type

defaultEnvironment =
    { environmentName = "new environment"
    , keyValues = []
    }

type alias Model a =
    { a
        | environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditIndex : Maybe Int
        , selectedEnvironmentToRenameIndex : Maybe Int
    }

getEnvironmentToEdit : Model a -> Maybe Type.Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Int -> Maybe Type.Environment
        selectEnvironment idx = List.getAt idx model.environments
    in
        Maybe.andThen selectEnvironment model.selectedEnvironmentToEditIndex


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
            updateEnv old = { old | environmentName = newEnvironmentName }
            mNewEnvs = List.updateAt idx updateEnv model.environments
      in
          case mNewEnvs of
              newEnvs ->
                  { model
                      | selectedEnvironmentToRenameIndex = Nothing
                      , environments = newEnvs
                  }

    EnvToRunMsg idx subMsg ->
        case getEnvironmentToEdit model of
            Nothing ->
                model
            Just environment ->
                case EnvToRun.update subMsg environment of
                    newEnvironment ->
                        let
                            newEnvironments = List.setAt idx newEnvironment model.environments
                        in
                            { model | environments = newEnvironments }
