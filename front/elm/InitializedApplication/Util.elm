module InitializedApplication.Util exposing (..)

import BuilderApp.Model as BuilderApp
import InitializedApplication.Model exposing (..)
import Util.List as List
import List.Extra as List
import Application.Type exposing (..)

-- todo: put this in app or model

type alias Environment =
    { id : Int
    , name : Editable String
    , showRenameInput : Bool
    , keyValues : Editable (List KeyValue)
    }

replaceEnvironmentToEdit : Model -> Environment -> Model
replaceEnvironmentToEdit model newEnvironment =
    let newEnvironments =
            case model.selectedEnvironmentToEditId of
                Just idx ->
                    List.updateListAt model.environments idx (\formerEnvironment -> newEnvironment)
                Nothing ->
                    model.environments
    in
        { model | environments = newEnvironments }
