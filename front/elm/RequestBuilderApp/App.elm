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
import RequestBuilderApp.RequestTree.App as RequestTree
import RequestBuilderApp.RequestTree.Util as RequestTree
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = BuilderMsg RequestBuilder.Msg
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

        BuilderMsg subMsg ->
            case getBuilder model of
                Just builder ->
                    let
                        (RequestCollection requestCollectionId requestNodes) =
                            model.requestCollection

                        ( newBuilder, newSubMsg ) =
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
                    ( newModel, newMsg )

                _ ->
                    ( model, Cmd.none )



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
        RequestFolder f ->
            RequestFolder f

        RequestFile f ->
            RequestFile (convertFromBuilderToFile builder)



-- * view


view : Model a -> Maybe Uuid -> Element Msg
view model fromScenarioId =
    let
        enableRunnerBanner =
            row [ width fill
              , paddingXY 0 10
              , centerY, centerX
              , Background.color secondaryColor
              , Font.color primaryColor
              , Font.center
              ]
              [ el [ centerX ] (text "Offline mode - Run the ")
              , link [ centerX ] { label = el [ Font.underline ] (text "Patchgirl Runner App")
                                 , url = href (DocumentationPage PatchGirlRunnerAppDoc)
                                 }
              , el [ centerX ] (text " to have the best experience!")
              ]
    in
    column [ width fill, spacing 10 ]
        [ if not model.runnerRunning then enableRunnerBanner else none
        , wrappedRow
              [ width fill
              , paddingXY 10 0
              , spacing 10
              ]
              [ column
                    [ alignTop
                    , spacing 20
                    , centerX
                    , padding 20
                    , width (fillPortion 1)
                    , Background.color white
                    , boxShadow
                    ]
                    [ el [] <| envSelectionView <| List.map .name model.environments
                    , el [ paddingXY 10 0 ] (map TreeMsg (RequestTree.view model))
                    ]
              , builderView model fromScenarioId
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


builderView : Model a -> Maybe Uuid -> Element Msg
builderView model fromScenarioId =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9), centerX, alignTop ]
                (map BuilderMsg (RequestBuilder.view builder fromScenarioId))

        Nothing ->
            el [ width (fillPortion 9), centerX, alignTop ]
                <| el ( [ centerX
                        , alignTop
                        , padding 20
                        , spacing 10
                        ] ++ boxAttrs
                      ) (text "No request selected")
