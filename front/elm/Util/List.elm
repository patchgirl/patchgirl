module Util.List exposing (..)

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
