module Application.App exposing (..)

import Http as Http
import Api.Client as Client

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
        Ok (Client.RequestCollection id requestNodes) ->
            ServerSuccess requestNodes

        Err error ->
            ServerError

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ServerSuccess requestNodes ->
            let
                defaultInitializedModel = InitializedApplication.defaultModel
                newModel =
                    Initialized { defaultInitializedModel
                                    | requestCollection = []
                                }
            in
                (newModel, Cmd.none)

        ServerError ->
            (model, Cmd.none)

        InitializedApplicationMsg subMsg ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
