module Application.App exposing (..)

import Http as Http
import Api.Client as Client
import Api.Converter as Client

import Application.View exposing(..)
import Application.Model exposing(..)
import Application.Message exposing(..)

import InitializedApplication.Model as InitializedApplication

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
                    Client.convertRequestCollectionFromBackToFront requestCollection
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
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
