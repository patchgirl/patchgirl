module PGBuilderApp.PGBuilder.App exposing (..)

import Animation
import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Json.Decode as Json
import Json.Print as Json
import List.Extra as List
import PrivateAddress exposing (..)
import RequestComputation exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)
import Runner


-- * model


type alias Model =
    { --id : Uuid
    -- , requestCollectionId : Int
--    , name : Editable String
    sqlQuery : Editable String
--    , httpMethod : Editable HttpMethod
--    , httpHeaders : Editable (List ( String, String ))
--    , httpBody : Editable String
--    , requestComputationResult : Maybe RequestComputationResult
--    , showResponseView : Bool
--    , whichResponseView : HttpResponseView
--    , runRequestIconAnimation : Animation.State
--    , runnerRunning : Bool
    }



-- * message


type Msg
    = UpdateSqlQuery String
    | AskRun
    | RemoteComputationDone RequestComputationResult -- request ran from the server
    | RemoteComputationFailed
    | ServerError
    | AskSave
    | SaveSuccessfully
    | Animate Animation.Msg
    | ShowBodyResponseView
    | ShowHeaderResponseView


-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSqlQuery newSqlQuery ->
            let
                newModel =
                    { model | sqlQuery = changeEditedValue newSqlQuery model.sqlQuery }
            in
            (newModel, Cmd.none)

        _ ->
            (model, Cmd.none)



-- * util
-- * view


view : Model -> Element Msg
view model =
    wrappedRow [ alignTop, width fill, spacing 10 ]
        [ el [ width (fillPortion 1)
             , alignTop
             , Background.color white
             , boxShadow
             , padding 20
             ] (builderView model)
        , el [ width (fillPortion 1)
             , Background.color white
             , boxShadow
             , alignTop
             , padding 30
             ] (responseView model)
        ]

builderView : Model -> Element Msg
builderView model =
    el [ alignLeft, width fill ] <|
        Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateSqlQuery
            , text = editedOrNotEditedValue model.sqlQuery
            , placeholder = Just <| Input.placeholder [] (text "SELECT * FROM your_table;")
            , label = labelInputView "Postgres SQL: "
            }


responseView : Model -> Element Msg
responseView model =
    none


-- * util

labelInputView : String -> Input.Label Msg
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
