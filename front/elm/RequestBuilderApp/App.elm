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
import Banner exposing (..)


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
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
            let
                ( newModel, newSubMsg ) =
                    RequestBuilder.update subMsg model
            in
            ( newModel, Cmd.map BuilderMsg newSubMsg )


-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, height fill, spacing 10 ]
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
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  map BuilderMsg (RequestBuilder.view model)
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
