module Util.KeyValue.Util exposing (..)

import Util.KeyValue.Model exposing (..)

delete : Int -> List(Model) -> List(Model)
delete idx models =
  case (idx, models) of
    (0, elem :: tail) -> tail
    (_, []) -> models
    (_, elem :: tail) -> elem :: (delete (idx - 1) tail)

changeKey : String -> Model -> Model
changeKey newKey model =
  { model | key = newKey }

changeValue : String -> Model -> Model
changeValue newValue model =
    { model | value = newValue }

modify : (Model -> Model) -> Int -> List(Model) -> List(Model)
modify f idx elems =
  case (idx, elems) of
    (0, elem :: tail) -> f elem :: tail
    (_, []) -> elems
    (_, elem :: tail) -> elem :: (modify f (idx - 1) tail)

modify2 : (Model -> Model) -> Int -> List(Model) -> List(Model)
modify2 f idx elems =
  case (idx, elems) of
    (0, elem :: tail) -> f elem :: tail
    (_, []) -> elems
    (_, elem :: tail) -> elem :: (modify f (idx - 1) tail)
