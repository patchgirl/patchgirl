module PGBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List
import Page exposing (..)
import PGBuilderApp.PGBuilder.App as PGBuilder
import PGBuilderApp.RequestTree.App as RequestTree
import PGBuilderApp.RequestTree.Util as RequestTree
import Util exposing (..)
import Uuid exposing (Uuid)



-- * model


type alias Model a =
    { a
        | page : Page
        , environments : List Environment
    }



-- * message


type Msg
    = BuilderMsg PGBuilder.Msg
    | TreeMsg RequestTree.Msg
    | EnvSelectionMsg Int



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- * util


-- * view


view : Model a -> Maybe Uuid -> Element Msg
view model fromScenarioId =
    wrappedRow
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
            , el [ paddingXY 10 0 ] none -- (map TreeMsg (RequestTree.view model))
            ]
        , none -- builderView model fromScenarioId
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


{-builderView : Model a -> Maybe Uuid -> Element Msg
builderView model fromScenarioId =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9)
               , width fill
               , height fill
               , alignTop
               ]
                (map BuilderMsg (PGBuilder.view builder fromScenarioId))

        Nothing ->
            el [ width (fillPortion 9)
               , centerX, centerY, alignTop
               ]
                <| el ( [ centerX
                        , alignTop
                        , padding 20
                        , spacing 10
                        ] ++ boxAttrs
                      ) (text "No request selected")
-}
