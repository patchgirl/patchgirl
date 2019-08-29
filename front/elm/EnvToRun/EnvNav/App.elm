module EnvToRun.EnvNav.App exposing (..)

import List.Extra as List

import EnvToRun.EnvNav.Message exposing (Msg(..))

import EnvToRun.App as EnvToRun

defaultEnvironment =
    { environmentName = "new environment"
    , keyValues = []
    }

type alias Environment =
    { environmentName : String
    , keyValues : List(String, String)
    }

type alias Model a =
    { a
        | environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditIndex : Maybe Int
        , selectedEnvironmentToRenameIndex : Maybe Int
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
      { model | environments = model.environments ++ [ defaultEnvironment ] }

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
      model
      {-
      case List.getAt idx model.environments of
        Nothing ->
            model
        Just { environmentName, env } ->
          case EnvToRun.update subMsg env of
            newEnvToRun ->
              let
                newEnvToRuns = List.setAt idx { name = name, env = newEnvToRun } model.envs
              in
                { model | envs = newEnvToRuns }
-}
