module PGBuilderApp.PGTree.Util exposing (..)

import Application.Type exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)


findNode : List PgNode -> Uuid -> Maybe PgNode
findNode pgNodes id =
    let
        find : PgNode -> Maybe PgNode
        find pgNode =
            case pgNode of
                (File file) as node ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (Folder folder) as node ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            let
                                (Children2 children) =
                                    folder.children
                            in
                            findNode children id
    in
    List.head <| catMaybes (List.map find pgNodes)


findFile : List PgNode -> Uuid -> Maybe PgFileRecord
findFile pgNodes id =
    case findNode pgNodes id of
        Just (File file) ->
            Just file

        _ ->
            Nothing
