module BuilderApp.Builder.App exposing (..)

import Browser
import Http
import Debug
import Time
import Url
import Json.Decode as Json
import List.Extra as List
import Combine as Combine
import Regex as Regex
import Uuid
import Api.Generated as Client
import Api.Converter as Client
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
import Animation


-- * model


type alias Model =
    { id : Uuid.Uuid
    , requestCollectionId : Int
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List (String, String))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationResult
    , showResponseView : Bool
    , runRequestIconAnimation : Animation.State
    }


-- * message


type Msg
  = UpdateUrl String
  | SetHttpMethod HttpMethod
  | UpdateHeaders String
  | SetHttpBody String
  | SetHttpBodyResponse String
  | AddHeaderInput
  | DeleteHeader Int
  | AskRun
  | LocalComputationDone (Result DetailedError ( Http.Metadata, String )) -- request ran from the browser
  | RemoteComputationDone RequestComputationResult -- request ran from the server
  | RemoteComputationFailed
  | ServerError
  | AskSave
  | SaveSuccessfully
  | Animate Animation.Msg


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
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
            let
                payload : Client.UpdateRequestFile
                payload =
                    { updateRequestFileName = editedOrNotEditedValue model.name
                    , updateRequestFileHttpUrl = editedOrNotEditedValue model.httpUrl
                    , updateRequestFileHttpMethod = Client.convertMethodFromFrontToBack (editedOrNotEditedValue model.httpMethod)
                    , updateRequestFileHttpHeaders = editedOrNotEditedValue model.httpHeaders
                    , updateRequestFileHttpBody = editedOrNotEditedValue model.httpBody
                    }

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdByRequestNodeId "" "" 1 model.id payload updateRequestFileResultToMsg
            in
                (model, newMsg)

        SaveSuccessfully ->
            let
                newModel =
                    { model
                        | name = NotEdited (editedOrNotEditedValue model.name)
                        , httpUrl = NotEdited (editedOrNotEditedValue model.httpUrl)
                        , httpMethod = NotEdited (editedOrNotEditedValue model.httpMethod)
                        , httpHeaders = NotEdited (editedOrNotEditedValue model.httpHeaders)
                        , httpBody = NotEdited (editedOrNotEditedValue model.httpBody)
                    }
            in
                (newModel, Cmd.none)

        AskRun ->
            let
                newMsg =
                    buildRequestToRun model.keyValues model

                newRunRequestIconAnimation =
                    Animation.interrupt [ Animation.loop [ Animation.to [ Animation.rotate (Animation.turn -0.05) ]
                                                         , Animation.to [ Animation.rotate (Animation.turn 0.05) ]
                                                         ]
                                        ] model.runRequestIconAnimation

                newModel =
                    { model
                        | showResponseView = True
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
                (newModel, newMsg)

        RemoteComputationDone remoteComputationResult ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt [ Animation.to [ Animation.rotate (Animation.turn 0) ]
                                        ] model.runRequestIconAnimation

                newModel =
                    { model
                        | requestComputationResult = Just remoteComputationResult
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
                (newModel, Cmd.none)

        RemoteComputationFailed ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt [ Animation.to [ Animation.rotate (Animation.turn 0) ]
                                        ] model.runRequestIconAnimation

                newModel =
                    { model
                        | requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
                (newModel, Cmd.none)

        LocalComputationDone result ->
            let
                newModel =
                    { model
                        | requestComputationResult = Just (convertResultToResponse result)
                    }
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

        AddHeaderInput ->
            let
                newHttpHeaders =
                    Edited (notEditedValue model.httpHeaders) (editedOrNotEditedValue model.httpHeaders ++ [ ("", "") ])

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
                (newModel, Cmd.none)

        DeleteHeader idx ->
            let
                editedHttpHeaders =
                    List.removeAt idx (editedOrNotEditedValue model.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders model.httpHeaders

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
                (newModel, Cmd.none)

        Animate subMsg ->
            let
                newModel =
                    { model
                        | runRequestIconAnimation = Animation.update subMsg model.runRequestIconAnimation
                    }
            in
                (newModel, Cmd.none)

        ServerError ->
            Debug.todo "server error"


-- * util


isBuilderDirty : Model -> Bool
isBuilderDirty model =
    isDirty model.httpMethod ||
        isDirty model.httpHeaders ||
            List.any isDirty [ model.name
                             , model.httpUrl
                             , model.httpBody
                             ]

updateRequestFileResultToMsg : Result Http.Error () -> Msg
updateRequestFileResultToMsg result =
    case result of
        Ok () ->
            SaveSuccessfully

        Err error ->
            ServerError


remoteComputationDoneToMsg : Result Http.Error Client.RequestComputationResult -> Msg
remoteComputationDoneToMsg result =
    case result of
        Ok backRequestComputationResult ->
            RemoteComputationDone <|
                Client.convertRequestComputationResultFromBackToFront backRequestComputationResult

        Err error ->
            RemoteComputationFailed

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

buildRequestToRun : List (Storable NewKeyValue KeyValue) -> Model -> Cmd Msg
buildRequestToRun envKeyValues builder =
    let
        request = buildRequestComputationInput envKeyValues builder
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
                    Client.postApiRequestComputation "" "" backRequestComputationInput remoteComputationDoneToMsg


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

buildRequestComputationInput : List (Storable NewKeyValue KeyValue) -> Model -> RequestComputationInput
buildRequestComputationInput envKeyValues builder =
    { scheme =
          schemeFromUrl (interpolate envKeyValues (editedOrNotEditedValue builder.httpUrl))
    , method =
        editedOrNotEditedValue builder.httpMethod
    , headers =
        List.map (interpolateHeader envKeyValues) (editedOrNotEditedValue builder.httpHeaders)
    , url =
        cleanUrl <|
            interpolate envKeyValues (editedOrNotEditedValue builder.httpUrl)
    , body =
        interpolate envKeyValues (editedOrNotEditedValue builder.httpBody)
    }

interpolateHeader : List (Storable NewKeyValue KeyValue) -> (String, String) -> (String, String)
interpolateHeader envKeyValues (headerKey, headerValue) =
    ( interpolate envKeyValues headerKey
    , interpolate envKeyValues headerValue
    )

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


interpolate : List (Storable NewKeyValue KeyValue) -> String -> String
interpolate envKeys str =
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
                "{{" ++ k ++ "}}"
  in
    case toRaw str of
      Ok templatedStrings -> List.map build templatedStrings |> List.foldr (++) ""
      Err errors -> Debug.log errors str

{-
 parse a string "hello {{user}} !" to a list of templated string
 eg: [ Sentence "hello "
     , Key "user"
     , Sentence " !"
     ]
-}
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
anychar = Combine.regex ".\n*" |> Combine.map Sentence

keyParser : Combine.Parser s TemplatedString
keyParser =
  let
    envKey : Combine.Parser s TemplatedString
    envKey = Combine.regex "([a-zA-Z]|[0-9]|-)+" |> Combine.map Key
  in
    Combine.between (Combine.string "{{") (Combine.string "}}") envKey


-- * view


view : Model -> Element Msg
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
                , headersView model
                , bodyView model
                ]
    in
        case model.showResponseView of
            False ->
                column [ width fill
                       , Background.color white
                       , boxShadow
                       , padding 20
                       ]
                    [ titleView model
                    , el [ width fill ] builderView
                    ]

            True ->
                wrappedRow [ alignTop, width fill, spacing 10 ]
                    [ column [ width (fillPortion 1)
                             , alignTop
                             , Background.color white
                             , boxShadow
                             , padding 20
                             ]
                          [ titleView model
                          , el [ alignTop ] builderView
                          ]
                    , el [ width (fillPortion 1)
                         , Background.color white
                         , boxShadow
                         , alignTop
                         , padding 30
                         ] (responseView model)
                    ]


-- ** title


titleView : Model -> Element Msg
titleView model =
    let
        name = notEditedValue model.name
    in
        row [ paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" (name) secondaryColor
            , mainActionButtonsView model
            ]

mainActionButtonsView : Model -> Element Msg
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

        runIcon : Element Msg
        runIcon =
            let
                attrs : List (Html.Attribute Msg)
                attrs =
                    Animation.render model.runRequestIconAnimation
            in
                iconWithTextAndColorAndAttr "send" "Run" primaryColor attrs

    in
        row rowParam
            [ Input.button inputParam
                { onPress = Just <| AskRun
                , label = el [ centerY ] runIcon
                }
            , case isBuilderDirty model of
                  True ->
                      Input.button inputParam
                          { onPress = Just <| AskSave
                          , label = el [ centerY ] <| iconWithTextAndColor "save" "Save" primaryColor
                          }

                  False ->
                      none
            ]


-- ** response


responseView : Model -> Element Msg
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


-- ** url


urlView : Model -> Element Msg
urlView model =
    el [ alignLeft, width fill ] <|
        Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = editedOrNotEditedValue model.httpUrl
            , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
            , label = labelInputView "Url: "
            }


-- ** method


methodView : Model -> Element Msg
methodView model =
    Input.radioRow [ padding 10, spacing 10 ]
        { onChange = SetHttpMethod
        , selected = Just <| editedOrNotEditedValue model.httpMethod
        , label = labelInputView "Method: "
        , options =
              [ Input.option HttpGet (text "Get")
              , Input.option HttpPost (text "Post")
              , Input.option HttpPut (text "Put")
              , Input.option HttpDelete (text "Delete")
              , Input.option HttpPatch (text "Patch")
              , Input.option HttpHead (text "Head")
              , Input.option HttpOptions (text "Options")
              ]
        }


-- ** header


headersView : Model -> Element Msg
headersView model =
    let
        headerInputs =
            List.indexedMap (headerView model) (editedOrNotEditedValue model.httpHeaders)

        addHeaderButton =
            Input.button [ centerX ]
                { onPress = Just <| AddHeaderInput
                , label =
                    row [ centerX ]
                        [ addIcon
                        , el [] (text "Add Header")
                        ]
                }
    in
        column [ width fill, spacing 10 ]
            [ text "Header:"
            , column [ width fill, spacing 10 ] headerInputs
            , addHeaderButton
            ]

headerView : Model -> Int -> (String, String) -> Element Msg
headerView model idx (headerKey, headerValue) =
    row [ width fill, spacing 10 ]
        [ Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = headerKey
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] (text "key: ")
            }
        , Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = headerValue
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] (text "value: ")
            }
        , Input.button [ centerY ]
            { onPress = Just <| DeleteHeader idx
            , label =
                row [ centerX, centerY ]
                    [ deleteIcon
                    ]
            }
        ]


-- ** body


bodyView : Model -> Element Msg
bodyView model =
    Input.multiline []
        { onChange = SetHttpBody
        , text = editedOrNotEditedValue model.httpBody
        , placeholder = Just <| Input.placeholder [] (text "{}")
        , label = labelInputView "Body: "
        , spellcheck = False
        }


-- ** util


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

joinTuple : String -> (String, String) -> String
joinTuple separator (key, value) =
    key ++ separator ++ value


-- * subscriptions


type alias ModelSubscriptions a =
    { a | runRequestIconAnimation : Animation.State }

subscriptions : ModelSubscriptions a -> Sub Msg
subscriptions model =
    Sub.batch [ Animation.subscription Animate [ model.runRequestIconAnimation ]
              ]
