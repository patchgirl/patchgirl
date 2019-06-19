module Window.Util exposing (..)

import BuilderApp.Model as BuilderApp
import Util.List as List

updateBuilders : Int -> List BuilderApp.Model -> (BuilderApp.Model -> BuilderApp.Model) -> List BuilderApp.Model
updateBuilders idx list f =
    List.updateListAt list idx f

replaceBuilder : Int -> List BuilderApp.Model -> BuilderApp.Model -> List BuilderApp.Model
replaceBuilder idx list newBuilder =
    updateBuilders idx list <| \_ -> newBuilder
