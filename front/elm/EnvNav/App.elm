module EnvNav.App exposing (..)

import List.Extra as List

import EnvNav.Model exposing (..)
import EnvNav.Message exposing (Msg(..))

import EnvApp.App as EnvApp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select idx ->
      ( { model | selectedEnvIndex = Just idx }, Cmd.none)

    Delete idx ->
      let
        newEnvs = List.removeAt idx model.envs
        newSelectedEnvIndex =
          case model.selectedEnvIndex == Just idx of
            True -> Nothing
            False -> model.selectedEnvIndex

        newModel = { model | selectedEnvIndex = Debug.log "sindex" newSelectedEnvIndex
                   , envs = newEnvs }
      in
        (newModel, Cmd.none)

    Add ->
      ( { model | envs = model.envs ++ [ defaultEnvInfo ] }, Cmd.none)

    ShowRenameInput idx ->
      ( { model | renameEnvIdx = Just idx }, Cmd.none)

    Rename idx newName ->
      let
        updateEnv old = { old | name = newName }
        mNewEnvs = List.updateAt idx updateEnv model.envs
      in
        case mNewEnvs of
          newEnvs -> ( { model | renameEnvIdx = Nothing, envs = newEnvs }, Cmd.none)


    EnvAppMsg idx subMsg ->
      case List.getAt idx model.envs of
        Nothing -> (model, Cmd.none)
        Just { name, env } ->
          case EnvApp.update subMsg env of
            (newEnvApp, newSubMsg) ->
              let
                newEnvApps = List.setAt idx { name = name, env = newEnvApp } model.envs
              in
                ( { model | envs = newEnvApps }, Cmd.map (EnvAppMsg idx) newSubMsg )
