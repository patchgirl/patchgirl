module Main exposing (..)

import Browser
import Json.Encode as E
import Json.Decode as D
import Application.App as Application
import Application.Type exposing (..)
import Api.Converter as Client
import Api.Generated as Client

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
        mkLoadedData : Session -> List Environment -> RequestCollection -> Application.UserData
        mkLoadedData session environments requestCollection =
            { session = session
            , environments = environments
            , requestCollection = requestCollection
            }
    in
        D.map3 mkLoadedData
            (D.at ["session"] (D.map Client.convertSessionFromBackToFront Client.jsonDecSession))
            (D.at ["environments"] (D.map (List.map Client.convertEnvironmentFromBackToFront) (D.list Client.jsonDecEnvironment)))
            (D.at ["requestCollection"] (D.map Client.convertRequestCollectionFromBackToFront Client.jsonDecRequestCollection))
