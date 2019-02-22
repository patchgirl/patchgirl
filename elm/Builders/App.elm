module Builders.App exposing (..)

import Builders.Model exposing (..)
import Builders.Message exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab idx ->
      ( { model | selectedBuilderIndex = Just idx }, Cmd.none)

    _ ->
      (model, Cmd.none)
