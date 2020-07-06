module PGBuilderApp.RequestTree.Util exposing (..)

import Application.Type exposing (..)
import Util exposing (..)
import Uuid


findNode : List RequestNode -> Uuid.Uuid -> Maybe RequestNode
findNode requestNodes id =
    let
        find : RequestNode -> Maybe RequestNode
        find requestNode =
            case requestNode of
                (RequestFile file) as node ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (RequestFolder folder) as node ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            findNode folder.children id
    in
    List.head <| catMaybes (List.map find requestNodes)


findFile : List RequestNode -> Uuid.Uuid -> Maybe RequestFileRecord
findFile requestNodes id =
    case findNode requestNodes id of
        Just (RequestFile file) ->
            Just file

        _ ->
            Nothing
