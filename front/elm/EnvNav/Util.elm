module EnvNav.Util exposing (..)

import List.Extra as List

import EnvNav.Model exposing (..)

getSelectedEnvInfo : Model -> Maybe EnvInfo
getSelectedEnvInfo model =
  let
    getEnv idx = List.getAt idx model.envs
  in
    model.selectedEnvIndex |> Maybe.andThen getEnv
