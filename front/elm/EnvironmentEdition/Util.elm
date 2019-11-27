module EnvironmentEdition.Util exposing (..)

import EnvironmentEdition.Model exposing (..)
import Application.Type as Type
import List.Extra as List

getEnvironmentToEdit : Model a -> Maybe Type.Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Int -> Maybe Type.Environment
        selectEnvironment idx = List.getAt idx model.environments
    in
        Maybe.andThen selectEnvironment model.selectedEnvironmentToEditId
