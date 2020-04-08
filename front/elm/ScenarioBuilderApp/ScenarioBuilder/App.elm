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
import Modal exposing (Modal(..))
import Modal


-- * model


type alias Model =
    { notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , scenarioCollectionId : Uuid.Uuid
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    }


-- * message


type Msg
  = SelectHttpRequest
  | CloseModal


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
                (newModel, Cmd.none)

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


-- * modal


selectHttpRequestModal : RequestCollection -> Modal.Config Msg
selectHttpRequestModal requestCollection =
    { closeMessage = CloseModal
    , maskAttributes = []
    , containerAttributes = [ padding 10 ]
    , headerAttributes = []
    , bodyAttributes = []
    , footerAttributes = []
    , header = Just (text "select http request")
    , body = Just (text "please select a request from the following")
    , footer = Just (text "Confirm    Cancel")
    }

confirmDeleteFolderModal : Modal.Config Msg
confirmDeleteFolderModal =
    { closeMessage = CloseModal
    , maskAttributes = []
    , containerAttributes = [ padding 10 ]
    , headerAttributes = []
    , bodyAttributes = []
    , footerAttributes = []
    , header = Just (text "confirm delete folder")
    , body = Nothing
    , footer = Nothing
    }
