module Foo exposing (..)

type Node file
    = Folder { id : Int
             , children : List (Node file)
             }
    | File { file | id : Int }

type alias FooFile = { id : Int, foo : String }
type alias BarFile = { bar : Int }

foo : Node BarFile
foo = Folder { id = 1
             , children = [ File { id = 2, bar = 3 } ]
             }

e = findNode [foo] 1

findNode : List (Node file)
         -> Int
         -> Maybe (Node file)
findNode nodes idToFind =
    let
        find : (Node file) -> Maybe (Node file)
        find node =
            case node of
                (File file) ->
                    case file.id == idToFind of
                        True ->
                            Just node

                        False ->
                            Nothing

                (Folder folder) ->
                    case folder.id == idToFind of
                        True ->
                            Just node

                        False ->
                            findNode folder.children idToFind
    in
    List.head <| catMaybes (List.map find nodes)

catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity
