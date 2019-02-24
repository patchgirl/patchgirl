module EnvNav.App exposing (..)

import EnvNav.Model exposing (..)
import EnvNav.Message exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select index ->
      ( { model | selectedEnvIndex = Just index }, Cmd.none)

    EnvMsg _ ->
      (model, Cmd.none)
