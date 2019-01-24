module Postman.App exposing (..)

import Postman.Model exposing (Model)
import Postman.Message exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Import -> (model, Cmd.none)