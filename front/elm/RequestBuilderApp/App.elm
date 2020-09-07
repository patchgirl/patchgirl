module RequestBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List
import Page exposing (..)
import RequestBuilderApp.RequestBuilder.App as RequestBuilder
import RequestBuilderApp.RequestBuilder.App2 as RequestBuilder2
import RequestBuilderApp.RequestTree.App as RequestTree
import RequestBuilderApp.RequestTree.Util as RequestTree
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation
import Banner exposing (..)


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestId : Maybe Uuid
        , displayedRequestBuilderView : BuilderView Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
        , requestNewNode : NewNode
    }


-- * message


type Msg
    = BuilderMsg2 RequestBuilder2.Msg
    | TreeMsg RequestTree.Msg
    | EnvSelectionMsg Int


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        EnvSelectionMsg idx ->
            let
                newModel =
                    { model | selectedEnvironmentToRunIndex = Just idx }
            in
            ( newModel, Cmd.none )

        TreeMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    RequestTree.update subMsg model
            in
            ( newModel, Cmd.map TreeMsg newSubMsg )

        BuilderMsg2 subMsg ->
            let
                ( newModel, newSubMsg ) =
                    RequestBuilder2.update subMsg model
            in
            ( newModel, Cmd.map BuilderMsg2 newSubMsg )


-- * util


getBuilder : Model a -> Maybe RequestBuilder.Model
getBuilder model =
    let
        (RequestCollection requestCollectionId requestNodes) =
            model.requestCollection

        mFile : Maybe RequestFileRecord
        mFile =
            Maybe.andThen (RequestTree.findFile requestNodes) model.displayedRequestId
    in
    case (model.displayedRequestId, mFile ) of
        ( Just _, Just file ) ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model
            in
            Just (convertFromFileToBuilder file requestCollectionId keyValuesToRun model.runnerRunning model.notification)

        _ ->
            Nothing


convertFromFileToBuilder : RequestFileRecord -> Int -> List (Storable NewKeyValue KeyValue) -> Bool -> Maybe Notification -> RequestBuilder.Model
convertFromFileToBuilder file requestCollectionId keyValuesToRun runnerRunning notification =
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
    , whichResponseView = file.whichResponseView
    , runRequestIconAnimation = file.runRequestIconAnimation
    , runnerRunning = runnerRunning
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
    , whichResponseView = builder.whichResponseView
    , runRequestIconAnimation = builder.runRequestIconAnimation
    }


changeFileBuilder : RequestBuilder.Model -> RequestNode -> RequestNode
changeFileBuilder builder node =
    case node of
        Folder f ->
            Folder f

        File f ->
            File (convertFromBuilderToFile builder)



-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, height fill, spacing 10 ]
        [ if not model.runnerRunning then enableRunnerBanner else none
        , wrappedRow
              [ width fill, height fill
              , paddingXY 10 0
              , spacing 10
              ]
              [ column
                    [ alignTop
                    , spacing 20
                    , centerX
                    , padding 20
                    , width (fillPortion 1), height fill
                    , Background.color white
                    , boxShadow
                    ]
                    [ el [] <| envSelectionView <| List.map .name model.environments
                    , el [ paddingXY 10 0 ] (map TreeMsg (RequestTree.view model))
                    ]
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  map (BuilderMsg2) (RequestBuilder2.view model)
              ]
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
