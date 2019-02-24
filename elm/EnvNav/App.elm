module EnvNav.App exposing (..)

import List.Extra as List

import EnvNav.Model exposing (..)
import EnvNav.Message exposing (Msg(..))

import Env.App as Env

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select index ->
      ( { model | selectedEnvIndex = Just index }, Cmd.none)

    EnvMsg idx subMsg ->
      case List.getAt idx model.envs of
        Nothing -> (model, Cmd.none)
        Just { name, env } ->
          case Env.update subMsg env of
            (newEnv, newSubMsg) ->
              let
                newEnvs = List.setAt idx { name = name, env = newEnv } model.envs
              in
                ( { model | envs = newEnvs }, Cmd.map (EnvMsg idx) newSubMsg )
