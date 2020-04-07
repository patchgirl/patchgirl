module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Application.Type exposing (..)
import Uuid


-- * model


type alias Model =
    { notification : Maybe String
    , id : Uuid.Uuid
    , scenarioCollectionId : Uuid.Uuid
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
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
