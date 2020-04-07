module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input
import Application.Type exposing (..)
import Util exposing (..)
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
  = SelectHttpRequest


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        _ ->
            (model, Cmd.none)


-- * view


view : Model -> Element Msg
view model =
    el [ width fill, centerX ]
        (Input.button [ centerX ]
            { onPress = Just SelectHttpRequest
            , label = el [ centerX, centerY ] (iconWithTextAndColorAndAttr "send" "Select http request" primaryColor [])
            })
