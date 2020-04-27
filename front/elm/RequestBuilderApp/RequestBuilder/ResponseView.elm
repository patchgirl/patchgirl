module RequestBuilderApp.RequestBuilder.ResponseView
    exposing ( statusResponseView
             , bodyResponseText
             )

import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Util exposing (..)
import Dict
import Json.Print as Json



-- * response view


statusResponseView : RequestComputationOutput -> Element a
statusResponseView requestComputationOutput =
    let
        statusText =
            String.fromInt requestComputationOutput.statusCode

        statusLabel =
            if requestComputationOutput.statusCode >= 200 && requestComputationOutput.statusCode < 300 then
                labelSuccess statusText

            else if requestComputationOutput.statusCode >= 400 && requestComputationOutput.statusCode < 500 then
                labelWarning statusText

            else if requestComputationOutput.statusCode >= 500 then
                labelError statusText

            else
                labelWarning statusText
    in
    row [ spacing 5 ]
        [ text "status: ", statusLabel
        ]


-- * body response text


bodyResponseText : String -> Dict.Dict String String -> String
bodyResponseText body responseHeaders =
    case Dict.get "content-type" responseHeaders of
        Just contentType ->
            case String.contains "application/json" contentType of
                True ->
                    Result.withDefault body (Json.prettyString { indent = 4, columns = 4 } body)

                False ->
                    body

        _ ->
            body
