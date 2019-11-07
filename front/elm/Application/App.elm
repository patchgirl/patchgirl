module Application.App exposing (..)

import Http as Http
import Api.Client as Client
import Api.Converter as Client
import Html exposing (..)
import InitializedApplication.View as InitializedApplication

import InitializedApplication.Model as InitializedApplication
import InitializedApplication.Message as InitializedApplication
import InitializedApplication.App as InitializedApplication
import BuilderApp.Model as BuilderApp

type Msg
  = ServerSuccess BuilderApp.RequestCollection
  | ServerError
  | InitializedApplicationMsg InitializedApplication.Msg

type Model
    = Unitialized
    | Initialized InitializedApplication.Model

defaultModel : Model
defaultModel = Unitialized

init : () -> (Model, Cmd Msg)
init _ =
    let
        initializeState =
            Client.getRequestCollectionByRequestCollectionId "/" 1 httpResultToMsg
    in
        (defaultModel, initializeState)

httpResultToMsg : Result Http.Error Client.RequestCollection -> Msg
httpResultToMsg result =
    case result of
        Ok requestCollection ->
            let
                newRequestCollection =
                    Debug.log "converted" <| Client.convertRequestCollectionFromBackToFront requestCollection
            in
                ServerSuccess newRequestCollection

        Err error ->
            ServerError

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ServerSuccess requestCollection ->
            let
                newModel =
                    Initialized <|
                        InitializedApplication.createModel requestCollection
            in
                (newModel, Cmd.none)

        ServerError ->
            (model, Cmd.none)

        InitializedApplicationMsg subMsg ->
            case model of
                Unitialized ->
                    let
                        errorMsg = "InitializedApplicationMsg received with unitialized Application - This should never happen"
                    in
                        (model, Cmd.none)

                Initialized initializedApplication ->
                    let
                        (newInitializedApplication, newMsg) =
                            InitializedApplication.update subMsg initializedApplication
                    in
                        ( Initialized newInitializedApplication
                        , Cmd.map InitializedApplicationMsg newMsg
                        )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model =
    case model of
        Unitialized ->
            div [] [ text "loading" ]
        Initialized initializedApplication ->
            div []
                [ Html.map InitializedApplicationMsg (InitializedApplication.view initializedApplication)
                ]
