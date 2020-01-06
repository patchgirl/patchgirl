module BuilderApp.Builder.App exposing (..)

import Browser
import Http
import Debug
import Url
import Json.Decode as Json
import List.Extra as List
import Combine as Combine
import Regex as Regex

import Api.Generated as Client
import Api.Converter as Client
import BuilderApp.Builder.Model exposing (..)
import Maybe.Extra as Maybe
import Application.Type exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input

import ViewUtil exposing (..)

import Html as Html
import Html.Attributes as Html
import Html.Events as Html

import Util.View as Util
import Application.Type exposing (..)
import Dict as Dict
import Json.Print as Json

import PrivateAddress exposing (..)


-- * message


type Msg
  = UpdateUrl String
  | SetHttpMethod Method
  | UpdateHeaders String
  | SetHttpBody String
  | SetHttpBodyResponse String
  | AskRun
  | LocalComputationDone (Result DetailedError ( Http.Metadata, String )) -- request ran from the browser
  | RemoteComputationDone RequestComputationResult -- request ran from the server
  | ServerError
  | AskSave


-- * update


update : Msg -> List (Storable NewKeyValue KeyValue) -> List KeyValue -> Model a -> (Model a, Cmd Msg)
update msg envKeyValues varKeyValues model =
    case msg of
        UpdateUrl newHttpUrl ->
            let
                newModel =
                    { model | httpUrl = changeEditedValue newHttpUrl model.httpUrl }
            in
                (newModel, Cmd.none)

        SetHttpMethod newMethod ->
            let
                newModel =
                    { model | httpMethod = changeEditedValue newMethod model.httpMethod }
            in
                (newModel, Cmd.none)

        UpdateHeaders rawHeaders ->
            let
                newModel =
                    case parseHeaders rawHeaders of
                        httpHeaders ->
                            { model | httpHeaders = changeEditedValue httpHeaders model.httpHeaders }
            in
                (newModel, Cmd.none)

        SetHttpBody httpBody ->
            let
                newModel =
                    { model | httpBody = changeEditedValue httpBody model.httpBody }
            in
                (newModel, Cmd.none)

        AskSave ->
            (model, Cmd.none)

        AskRun ->
            let
                newMsg =
                    buildRequestToRun envKeyValues varKeyValues model
                newModel =
                    { model
                        | showResponseView = True
                    }
            in
                (newModel, Debug.log "test" newMsg)

        LocalComputationDone result ->
            let
                newModel =
                    { model | requestComputationResult = Just (convertResultToResponse result) }
            in
                (newModel, Cmd.none)

        RemoteComputationDone remoteComputationResult ->
            let
                newModel =
                    { model | requestComputationResult = Just remoteComputationResult }
            in
                (newModel, Cmd.none)

        SetHttpBodyResponse newBody ->
            case model.requestComputationResult of
                Just (GotRequestComputationOutput response) ->
                    let
                        newRequestComputationOutput =
                            { response | body = newBody }
                        newModel =
                            { model | requestComputationResult = Just (GotRequestComputationOutput newRequestComputationOutput) }
                    in
                        (newModel, Cmd.none)

                _ ->
                    (model, Cmd.none)

        ServerError ->
            Debug.todo "server error"


-- * util


remoteComputationDoneToMsg : Result Http.Error Client.RequestComputationResult -> Msg
remoteComputationDoneToMsg result =
    case result of
        Ok backRequestComputationResult ->
            RemoteComputationDone <|
                Client.convertRequestComputationResultFromBackToFront backRequestComputationResult

        Err error ->
            Debug.log "test" ServerError

parseHeaders : String -> List(String, String)
parseHeaders headers =
  let
    parseRawHeader : String -> (String, String)
    parseRawHeader rawHeader =
      case String.split ":" rawHeader of
        [headerKey, headerValue] -> (headerKey, headerValue)
        _ -> ("", "")
  in
      String.lines headers |> List.map parseRawHeader

buildRequestToRun : List (Storable NewKeyValue KeyValue) -> List KeyValue -> Model a -> Cmd Msg
buildRequestToRun envKeyValues varKeyValues builder =
    let
        request = buildRequestComputationInput envKeyValues varKeyValues builder
    in
        case isPrivateAddress request.url of
            True ->
                let
                    cmdRequest =
                        { method = methodToString request.method
                        , headers = List.map mkHeader request.headers
                        , url = (schemeToString request.scheme) ++ "://" ++ request.url
                        , body = Http.stringBody "application/json" request.body
                        , expect = expectStringDetailed LocalComputationDone
                        , timeout = Nothing
                        , tracker = Nothing
                        }

                in
                    Http.request cmdRequest

            False ->
                let
                    backRequestComputationInput =
                        Client.convertRequestComputationInputFromFrontToFromBack request
                in
                    Client.postRequestComputation "" "" backRequestComputationInput remoteComputationDoneToMsg


type DetailedError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String

convertResponseStringToResult : Http.Response String -> Result DetailedError ( Http.Metadata, String )
convertResponseStringToResult httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )

convertResultToResponse : Result DetailedError (Http.Metadata, String) -> RequestComputationResult
convertResultToResponse result =
    case result of
        Err (BadUrl url) ->
            RequestBadUrl

        Err Timeout ->
            RequestTimeout

        Err NetworkError ->
            RequestNetworkError

        Err (BadStatus metadata body) ->
            GotRequestComputationOutput { statusCode = metadata.statusCode
                        , statusText = metadata.statusText
                        , headers = metadata.headers
                        , body = body
                        }

        Ok (metadata, body) ->
            GotRequestComputationOutput { statusCode = metadata.statusCode
                        , statusText = metadata.statusText
                        , headers = metadata.headers
                        , body = body
                        }

expectStringDetailed : (Result DetailedError ( Http.Metadata, String ) -> msg) -> Http.Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseStringToResult


-- * request runner


schemeToString : Scheme -> String
schemeToString scheme =
    case scheme of
        Http ->
            "http"

        Https ->
            "https"

buildRequestComputationInput : List (Storable NewKeyValue KeyValue) -> List KeyValue -> Model a -> RequestComputationInput
buildRequestComputationInput envKeyValues varKeyValues builder =
    { scheme =
          schemeFromUrl (editedOrNotEditedValue builder.httpUrl)
    , method =
        editedOrNotEditedValue builder.httpMethod
    , headers =
        editedOrNotEditedValue builder.httpHeaders
    , url =
        cleanUrl <|
            interpolate envKeyValues varKeyValues (editedOrNotEditedValue builder.httpUrl)
    , body =
        editedOrNotEditedValue builder.httpBody
    }


schemeFromUrl : String -> Scheme
schemeFromUrl url =
    case String.startsWith "https://" url of
        True ->
            Https

        False ->
            Http

mkHeader : (String, String) -> Http.Header
mkHeader (headerKey, headerValue) = Http.header headerKey headerValue

cleanUrl : String -> String
cleanUrl url =
    removeSchemeFromUrl (String.trimLeft url)

removeSchemeFromUrl : String -> String
removeSchemeFromUrl url =
    let
        schemeRegex : Regex.Regex
        schemeRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith { caseInsensitive = False
                                     , multiline = False
                                     } "^https?://"
    in
        Regex.replace schemeRegex (\_ -> "") url


interpolate : List (Storable NewKeyValue KeyValue) -> List KeyValue -> String -> String
interpolate envKeys varKeyValues str =
  let
    build : TemplatedString -> String
    build templatedString =
      case templatedString of
        Sentence c -> c
        Key k ->
          case List.find (\sEnvKey ->
                              case sEnvKey of
                                  New { key } ->
                                      key == k

                                  Saved { key } ->
                                      key == k

                                  Edited2 _ { key } ->
                                      key == k
                         ) envKeys of
            Just sEnvKey ->
                case sEnvKey of
                    New { value } ->
                        value

                    Saved { value } ->
                        value

                    Edited2 _ { value } ->
                        value
            Nothing ->
                case List.find (\varKeyValue -> varKeyValue.key == k) varKeyValues of
                    Just  { value } -> value
                    Nothing -> k
  in
    case toRaw str of
      Ok result -> List.map build result |> List.foldr (++) ""
      Err errors -> Debug.log errors str

toRaw : String -> Result String (List TemplatedString)
toRaw str =
  case Combine.parse templatedStringParser str of
    Ok (_, _, result) ->
      Ok result

    Err (_, stream, errors) ->
      Err (String.join " or " errors)

type TemplatedString
    = Sentence String
    | Key String

templatedStringParser : Combine.Parser s (List TemplatedString)
templatedStringParser = Combine.many (Combine.or keyParser anychar)

anychar : Combine.Parser s TemplatedString
anychar = Combine.regex "." |> Combine.map Sentence

keyParser : Combine.Parser s TemplatedString
keyParser =
  let
    envKey : Combine.Parser s TemplatedString
    envKey = Combine.regex "([a-zA-Z]|[0-9])+" |> Combine.map Key
  in
    Combine.between (Combine.string "{{") (Combine.string "}}") envKey


-- * method util


methodToString : Method -> String
methodToString method =
  case method of
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Patch -> "PATCH"
    Head -> "HEAD"
    Options -> "OPTIONS"

fromString : String -> Maybe Method
fromString method =
  case method of
    "GET" -> Just Get
    "POST" -> Just Post
    "PUT" -> Just Put
    "DELETE" -> Just Delete
    "PATCH" -> Just Patch
    "HEAD" -> Just Head
    "OPTIONS" -> Just Options
    _ -> Nothing


-- * view


view : Model a -> Element Msg
view model =
    let
        builderView =
            column [ width fill, spacing 10 ]
                [ column [ width fill ]
                      [ row [ width fill, spacing 10 ]
                            [ urlView model
                            ]
                      ]
                , methodView model
                , headerView model
                , bodyView model
                ]
    in
        case model.showResponseView of
            False ->
                column [ width fill ]
                    [ titleView model
                    , el [ width fill ] builderView
                    ]

            True ->
                column [ width fill ]
                    [ titleView model
                    , row [ width fill, spacing 20 ]
                        [ el [ width (fillPortion 1), alignTop ] builderView
                        , el [ width (fillPortion 1), alignTop ] (responseView model)
                        ]
                    ]

titleView : Model a -> Element Msg
titleView model =
    let
        name = notEditedValue model.name
    in
        row [ centerX, paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" (name) secondaryColor
            , mainActionButtonsView model
            ]

responseView : Model a -> Element Msg
responseView model =
    let
        bodyResponseText : String -> Dict.Dict String String -> String
        bodyResponseText body responseHeaders =
            case Dict.get "content-type" responseHeaders of
                Just contentType ->
                    case String.contains "application/json" contentType of
                        True -> Result.withDefault body (Json.prettyString { indent = 4, columns = 4 } body)
                        False -> body
                _ -> body

        statusResponseView : RequestComputationOutput -> Element Msg
        statusResponseView requestComputationOutput =
            let
                statusText =
                    String.fromInt requestComputationOutput.statusCode ++ " - " ++ requestComputationOutput.statusText
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
                        column [ spacing 5 ]
                            [ text "status: "
                            , statusLabel
                            ]

        headersResponseView : RequestComputationOutput -> Element Msg
        headersResponseView requestComputationOutput =
            let
                headers =
                    Dict.toList requestComputationOutput.headers
                        |> List.map (joinTuple ": ")
                        |> String.join "\n"
            in
                Input.multiline []
                    { onChange = SetHttpBody
                    , text = headers
                    , placeholder = Nothing
                    , label = labelInputView "Headers: "
                    , spellcheck = False
                    }

        bodyResponseView : RequestComputationOutput -> Element Msg
        bodyResponseView requestComputationOutput =
            case requestComputationOutput.body of
                "" ->
                    none

                _ ->
                    Input.multiline []
                        { onChange = SetHttpBodyResponse
                        , text = bodyResponseText requestComputationOutput.body requestComputationOutput.headers
                        , placeholder = Nothing                                , label = labelInputView "body: "
                        , spellcheck = False
                        }

        errorAttributes =
            [ centerX
            , centerY
            ]

    in
        case model.requestComputationResult of
            Nothing ->
                none

            Just (GotRequestComputationOutput requestComputationOutput) ->
                column [ spacing 10 ]
                    [ statusResponseView requestComputationOutput
                    , bodyResponseView requestComputationOutput
                    , headersResponseView requestComputationOutput
                    ]

            Just RequestTimeout ->
                el errorAttributes (text "timeout")

            Just RequestNetworkError ->
                el errorAttributes (text "network error")

            Just RequestBadUrl ->
                el errorAttributes (text "bad url")


urlView : Model a -> Element Msg
urlView model =
    el [ alignLeft, width fill ] <|
        Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = editedOrNotEditedValue model.httpUrl
            , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
            , label = labelInputView "Url: "
            }

mainActionButtonsView : Model a -> Element Msg
mainActionButtonsView model =
    let
        rowParam =
            [ centerY
            , height fill
            , spacing 10
            , alignRight
            , Font.color primaryColor
            ]

        inputParam =
            [ Border.solid
            , Border.color secondaryColor
            , Border.width 1
            , Border.rounded 5
            , alignBottom
            , Background.color secondaryColor
            , paddingXY 10 10
            ]

    in
        row rowParam
            [ Input.button inputParam
                { onPress = Just <| AskRun
                , label = el [ centerY ] <| iconWithTextAndColor "send" "Run" primaryColor
                }
            , case isBuilderDirty model of
                  True ->
                      Input.button inputParam
                          { onPress = Just <| AskSave
                          , label = el [ centerY] <| iconWithTextAndColor "save" "Save" primaryColor
                          }

                  False ->
                      none
            ]

methodView : Model a -> Element Msg
methodView model =
    Input.radioRow [ padding 10, spacing 20 ]
        { onChange = SetHttpMethod
        , selected = Just <| editedOrNotEditedValue model.httpMethod
        , label = labelInputView "Method: "
        , options =
              [ Input.option Get (text "Get")
              , Input.option Post (text "Post")
              , Input.option Put (text "Put")
              , Input.option Delete (text "Delete")
              , Input.option Patch (text "Patch")
              , Input.option Head (text "Head")
              , Input.option Options (text "Options")
              ]
        }

joinTuple : String -> (String, String) -> String
joinTuple separator (key, value) =
    key ++ separator ++ value

headerView : Model a -> Element Msg
headerView model =
    let
        headersToText : Editable (List (String, String)) -> String
        headersToText eHeaders =
            editedOrNotEditedValue model.httpHeaders
                |> List.map (joinTuple ":")
                |> String.join "\n"
    in
        Input.multiline []
            { onChange = UpdateHeaders
            , text = headersToText model.httpHeaders
            , placeholder = Just <| Input.placeholder [] (text "Header: SomeHeader\nHeader2: SomeHeader2")
            , label = labelInputView "Headers: "
            , spellcheck = False
            }

bodyView : Model a -> Element Msg
bodyView model =
    Input.multiline []
        { onChange = SetHttpBody
        , text = editedOrNotEditedValue model.httpBody
        , placeholder = Just <| Input.placeholder [] (text "{}")
        , label = labelInputView "Body: "
        , spellcheck = False
        }

labelInputView : String -> Input.Label Msg
labelInputView labelText =
    let
        size =
            width (fill
                  |> maximum 100
                  |> minimum 100
                  )
    in
        Input.labelAbove [ centerY, size ] <| text labelText
