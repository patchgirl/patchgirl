module ScenarioBuilderApp.App exposing (..)

import Uuid
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Element.Border as Border
import Element.Events as Events
import Html.Events as Html
import Html as Html
import Html.Attributes as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json

import Application.Type exposing (..)
import Page exposing(..)
import ScenarioBuilderApp.ScenarioBuilder.App as ScenarioBuilder
import ScenarioBuilderApp.ScenarioBuilderTree.App as ScenarioBuilderTree


-- * model


type alias Model a =
    { a
        | notification : Maybe String
        , scenarioCollection : ScenarioCollection
        , displayedScenarioNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
    }


-- * message


type Msg
  = ScenarioBuilderMsg ScenarioBuilder.Msg
  | ScenarioTreeMsg ScenarioBuilderTree.Msg
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

        ScenarioTreeMsg subMsg ->
            let
                (newModel, newSubMsg) = ScenarioBuilderTree.update subMsg model
            in
                (newModel, Cmd.map ScenarioTreeMsg newSubMsg)

        _ ->
            (model, Cmd.none)


-- * util


getSelectedBuilderId : Model a -> Maybe Uuid.Uuid
getSelectedBuilderId model =
    case model.page of
        ScenarioPage (Just id) ->
            Just id

        _ ->
            Nothing


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
              , el [ paddingXY 10 0 ] (map ScenarioTreeMsg (ScenarioBuilderTree.view model))
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
    none
