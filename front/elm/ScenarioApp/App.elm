module ScenarioApp.App exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Application.Type exposing (..)


-- ** model


type alias Model a =
    { a | session : Session
    }


-- ** message


type Msg
    = DoNothing


-- ** update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        DoNothing ->
            (model, Cmd.none)


-- ** view


view : Model a -> Element Msg
view model =
    none
