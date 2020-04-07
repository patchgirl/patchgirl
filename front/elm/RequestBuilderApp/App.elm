module RequestBuilderApp.App exposing (..)

import Uuid

import RequestBuilderApp.RequestTree.App as RequestTree
import RequestBuilderApp.RequestBuilder.App as RequestBuilder
import Util.Maybe as Maybe
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Application.Type exposing (..)
import EnvironmentToRunSelection.App as EnvSelection
import List.Extra as List
import Application.Model as Application

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Html.Events as Html
import Html as Html
import Html.Attributes as Html
import ViewUtil exposing (..)

import Json.Decode as Json
import Html.Events.Extra exposing (targetValueIntParse)
import Page exposing (..)


-- * model


type alias Model a =
    { a
        | notification : Maybe String
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
    }


-- * message


type Msg
  = BuilderMsg RequestBuilder.Msg
  | TreeMsg RequestTree.Msg
  | EnvSelectionMsg Int


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        EnvSelectionMsg idx ->
            let
                newModel =
                    { model | selectedEnvironmentToRunIndex = Just idx }
            in
                (newModel, Cmd.none)

        TreeMsg subMsg ->
            let
                (newModel, newSubMsg) = RequestTree.update subMsg model
            in
                (newModel, Cmd.map TreeMsg newSubMsg)

        BuilderMsg subMsg ->
            case getBuilder model of
                Just builder ->
                    let
                        (RequestCollection requestCollectionId requestNodes) =
                            model.requestCollection

                        (newBuilder, newSubMsg) =
                            RequestBuilder.update subMsg builder

                        newBuilderTree =
                            List.map (RequestTree.modifyRequestNode builder.id (changeFileBuilder newBuilder)) requestNodes

                        newModel =
                            { model
                                | requestCollection = RequestCollection requestCollectionId newBuilderTree
                                , notification = newBuilder.notification
                            }

                        newMsg =
                            Cmd.map BuilderMsg newSubMsg
                    in
                        (newModel, newMsg)

                _ ->
                    (model, Cmd.none)


-- * util


getSelectedBuilderId : Model a -> Maybe Uuid.Uuid
getSelectedBuilderId model =
    case model.page of
        ReqPage (Just id) ->
            Just id

        _ ->
            Nothing

getBuilder : Model a -> Maybe RequestBuilder.Model
getBuilder model =
    let
        (RequestCollection requestCollectionId requestNodes) = model.requestCollection
        mFile : Maybe RequestFileRecord
        mFile = Maybe.andThen (RequestTree.findFile requestNodes) (getSelectedBuilderId model)
    in
        case (getSelectedBuilderId model, mFile) of
            (Just _, Just file) ->
                let
                    keyValuesToRun =
                        (Application.getEnvironmentKeyValuesToRun model)

                in
                    Just (convertFromFileToBuilder file requestCollectionId keyValuesToRun model.notification)

            _ ->
                Nothing

convertFromFileToBuilder : RequestFileRecord -> Int -> List (Storable NewKeyValue KeyValue) -> Maybe String -> RequestBuilder.Model
convertFromFileToBuilder file requestCollectionId keyValuesToRun notification =
    { notification = notification
    , id = file.id
    , requestCollectionId = requestCollectionId
    , keyValues = keyValuesToRun
    , name = file.name
    , httpUrl = file.httpUrl
    , httpMethod = file.httpMethod
    , httpHeaders = file.httpHeaders
    , httpBody = file.httpBody
    , requestComputationResult = file.requestComputationResult
    , showResponseView = file.showResponseView
    , runRequestIconAnimation = file.runRequestIconAnimation
    }

convertFromBuilderToFile : RequestBuilder.Model -> RequestFileRecord
convertFromBuilderToFile builder =
    { id = builder.id
    , name = builder.name
    , httpUrl = builder.httpUrl
    , httpMethod = builder.httpMethod
    , httpHeaders = builder.httpHeaders
    , httpBody = builder.httpBody
    , requestComputationResult = builder.requestComputationResult
    , showResponseView = builder.showResponseView
    , runRequestIconAnimation = builder.runRequestIconAnimation
    }

changeFileBuilder : RequestBuilder.Model -> RequestNode -> RequestNode
changeFileBuilder builder node =
    case node of
        RequestFolder f ->
            RequestFolder f
        RequestFile f ->
            RequestFile (convertFromBuilderToFile builder)


-- * view


view : Model a -> Element Msg
view model =
    wrappedRow [ width fill
               , paddingXY 10 0
               , spacing 10
               ]
        [ column [ alignTop
                 , spacing 20
                 , centerX
                 , padding 20
                 , width (fillPortion 1)
                 , Background.color white
                 , boxShadow
                 ]
              [ el [ ] <| envSelectionView <| List.map .name model.environments
              , el [ paddingXY 10 0 ] (map TreeMsg (RequestTree.view model))
              ]
        , el [ alignTop, width (fillPortion 9) ]
            <| builderView model (getSelectedBuilderId model)
        ]

envSelectionView : List (Editable String) -> Element Msg
envSelectionView environmentNames =
    let
        entryView : Int -> Editable String -> Html.Html Msg
        entryView idx envName =
            Html.option [ Html.value (String.fromInt idx) ] [ Html.text (editedOrNotEditedValue envName) ]
    in
        html <|
            Html.div []
                [ Html.label [] [ Html.text "Env: " ]
                , Html.select [ Html.on "change" (Json.map EnvSelectionMsg targetValueIntParse) ]
                    (List.indexedMap entryView environmentNames)
                ]

builderView : Model a -> Maybe Uuid.Uuid -> Element Msg
builderView model mId =
    case getBuilder model of
        Just builder ->
            el [ width fill, height fill, spacing 20 ]
                (map BuilderMsg (RequestBuilder.view builder))

        Nothing ->
            el [ centerX ] (text "No request selected")
