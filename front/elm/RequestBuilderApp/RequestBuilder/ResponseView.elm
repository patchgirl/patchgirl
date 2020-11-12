module RequestBuilderApp.RequestBuilder.ResponseView
    exposing ( statusResponseView
             , headersRequestView
             , bodyRequestView
             , bodyResponseView
             , headersResponseView
             )

import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Util exposing (..)
import Dict
import Json.Print as Json


-- * status


statusResponseView : RequestComputation -> Element a
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
        [ text "Status: ", statusLabel
        ]


-- * body view


bodyRequestView : RequestComputation -> (String -> a) -> Element a
bodyRequestView requestComputationOutput msg =
    Input.multiline [ scrollbars, width fill, height fill ]
        { onChange = msg
        , text = bodyText requestComputationOutput.requestBody requestComputationOutput.headers
        , placeholder = Nothing
        , label = labelInputView "body: "
        , spellcheck = False
        }

bodyResponseView : RequestComputation -> (String -> a) -> Element a
bodyResponseView requestComputationOutput msg =
    Input.multiline [ scrollbars, width fill, height fill ]
        { onChange = msg
        , text = bodyText requestComputationOutput.body requestComputationOutput.headers
        , placeholder = Nothing
        , label = labelInputView "body: "
        , spellcheck = False
        }

-- * header view


headersRequestView : RequestComputation -> (String -> a) -> Element a
headersRequestView requestComputationOutput msg =
    let
        headers =
            Dict.toList requestComputationOutput.requestHeaders
                |> List.map (joinTuple ": ")
                |> String.join "\n"
    in
    Input.multiline [ clipX ]
        { onChange = msg
        , text = headers
        , placeholder = Nothing
        , label = labelInputView "Headers: "
        , spellcheck = False
        }

headersResponseView : RequestComputation -> (String -> a) -> Element a
headersResponseView requestComputationOutput msg =
    let
        headers =
            Dict.toList requestComputationOutput.headers
                |> List.map (joinTuple ": ")
                |> String.join "\n"
    in
    Input.multiline [ clipX ]
        { onChange = msg
        , text = headers
        , placeholder = Nothing
        , label = labelInputView "Headers: "
        , spellcheck = False
        }


-- * util


labelInputView : String -> Input.Label a
labelInputView labelText =
    let
        size =
            width
                (fill
                    |> maximum 100
                    |> minimum 100
                )
    in
    Input.labelAbove [ centerY, size ] <| text labelText

joinTuple : String -> ( String, String ) -> String
joinTuple separator ( key, value ) =
    key ++ separator ++ value

bodyText : String -> Dict.Dict String String -> String
bodyText body responseHeaders =
    case Dict.get "content-type" responseHeaders of
        Just contentType ->
            case String.contains "application/json" contentType of
                True ->
                    Result.withDefault body (Json.prettyString { indent = 4, columns = 4 } body)

                False ->
                    body

        _ ->
            body
