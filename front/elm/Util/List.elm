module Util.List exposing (..)

import List.Extra as List

flatten : List (Maybe a) -> List a
flatten l =
  let
    f : Maybe a -> List a -> List a
    f mA list =
      case mA of
        Just a -> list ++ [ a ]
        Nothing -> list
  in
    List.foldl f [] l

--  useful when drag and dropping elem from a list to another index
changePlace : Int -> Int -> a -> List(a) -> List(a)
changePlace newIdx modelToMoveId modelToMove list =
    let
        replace id elem =
            case id > modelToMoveId of
                True -> [ elem, modelToMove ]
                False -> [ modelToMove, elem ]
        move : Int -> a -> List(a) -> List(a)
        move id elem acc =
            case modelToMoveId == id of
                True -> acc
                False ->
                    case newIdx == id of
                        True -> acc ++ replace id elem
                        False -> acc ++ [ elem ]
    in
        List.indexedFoldl move [] list
