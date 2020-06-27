module Main exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.App as Application
import Application.Model as Application
import Application.Type exposing (..)
import Browser
import Json.Decode as D
import Json.Encode as E


main : Program E.Value Application.Model Application.Msg
main =
    Browser.application
        { init = decodeLoadedData >> Application.init
        , update = Application.update
        , subscriptions = Application.subscriptions
        , view = Application.view
        , onUrlRequest = Application.LinkClicked
        , onUrlChange = Application.UrlChanged
        }


decodeLoadedData : E.Value -> Application.UserData
decodeLoadedData json =
    case D.decodeValue loadedDataDecoder json of
        Ok userData ->
            userData

        Err str ->
            Debug.todo "failed to load user data, try reloading the page"


loadedDataDecoder : D.Decoder Application.UserData
loadedDataDecoder =
    let
        mkLoadedData : Session -> List Environment -> RequestCollection -> ScenarioCollection -> Application.UserData
        mkLoadedData session environments requestCollection scenarioCollection =
            { session = session
            , environments = environments
            , requestCollection = requestCollection
            , scenarioCollection = scenarioCollection
            }
    in
    D.map4 mkLoadedData
        (D.at [ "session" ] (D.map Client.convertSessionFromBackToFront Client.jsonDecSession))
        (D.at [ "environments" ] (D.map (List.map Client.convertEnvironmentFromBackToFront) (D.list Client.jsonDecEnvironment)))
        (D.at [ "requestCollection" ] (D.map Client.convertRequestCollectionFromBackToFront Client.jsonDecRequestCollection))
        (D.at [ "scenarioCollection" ] (D.map Client.convertScenarioCollectionFromBackToFront Client.jsonDecScenarioCollection))
