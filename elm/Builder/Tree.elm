module Builder.Tree exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Builder.Message exposing (Msg(..))
import Builder.Url

type Model = Folder String Model | File String

view : String -> Html Msg
view model =
  let
    a = 1
  in
    div [ id "tree" ]
      [ text model ]
