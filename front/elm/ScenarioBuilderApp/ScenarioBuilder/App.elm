module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events


-- * model


type alias Model =
    {
    }


-- * message


type Msg
  = DoNothing


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        _ ->
            (model, Cmd.none)


-- * view


view : Model -> Element Msg
view model =
    none


-- * subscriptions
