module Window.Util exposing (..)

import BuilderApp.Model as BuilderApp
import Window.Model exposing (..)
import Util.List as List
import List.Extra as List

updateBuilders : Int -> List BuilderApp.Model -> (BuilderApp.Model -> BuilderApp.Model) -> List BuilderApp.Model
updateBuilders idx list f =
    List.updateListAt list idx f

-- TODO remove function
getSelectedBuilder : Model -> BuilderApp.Model
getSelectedBuilder model =
    model.buildersAppModel

type alias Environment =
    { name : String
    , keyValues : List(String, String)
    }

replaceEnvironmentToEdit : Model -> Environment -> Model
replaceEnvironmentToEdit model newEnvironment =
    let newEnvironments =
            case model.selectedEnvironmentToEditIndex of
                Just idx ->
                    List.updateListAt model.environments idx (\formerEnvironment -> newEnvironment)
                Nothing ->
                    model.environments
    in
        { model | environments = newEnvironments }
