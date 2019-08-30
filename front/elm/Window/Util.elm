module Window.Util exposing (..)

import BuilderApp.Model as BuilderApp
import Window.Model exposing (..)
import Util.List as List
import List.Extra as List

updateBuilders : Int -> List BuilderApp.Model -> (BuilderApp.Model -> BuilderApp.Model) -> List BuilderApp.Model
updateBuilders idx list f =
    List.updateListAt list idx f

replaceBuilder : Maybe Int -> List BuilderApp.Model -> BuilderApp.Model -> List BuilderApp.Model
replaceBuilder mIdx list newBuilder =
    case mIdx of
        Just idx -> updateBuilders idx list <| \_ -> newBuilder
        Nothing -> Debug.log "Could not update builder, this should never happen" list

getSelectedBuilder : Model -> Maybe BuilderApp.Model
getSelectedBuilder model =
    Maybe.andThen (\idx -> List.getAt idx model.buildersAppModel) model.selectedWorkspaceIndex

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
