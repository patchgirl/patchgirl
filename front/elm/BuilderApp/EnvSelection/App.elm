module BuilderApp.EnvSelection.App exposing (..)

import BuilderApp.EnvSelection.Model exposing (..)
import BuilderApp.EnvSelection.Message exposing (Msg(..))

import List.Extra as List

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select idx ->
      case List.getAt idx model.envs of
        Just _ -> ( { model | selectedEnvIdx = Just idx }, Cmd.none)
        Nothing -> ( { model | selectedEnvIdx = Nothing }, Cmd.none)
