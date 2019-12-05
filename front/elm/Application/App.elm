module Application.App exposing (..)

import Http as Http
import Api.Client as Client
import Api.Converter as Client
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Html as Html
import InitializedApplication.View as InitializedApplication

import InitializedApplication.Model as InitializedApplication
import InitializedApplication.Message as InitializedApplication
import InitializedApplication.App as InitializedApplication
import BuilderApp.Model as BuilderApp
import Application.Type exposing (..)


-- * model


type Model
    = Unitialized
    | SignedInNotInitialized
    | Pending
      { mRequestCollection : Maybe BuilderApp.RequestCollection
      , mEnvironments : Maybe (List Environment)
      }
    | Initialized InitializedApplication.Model

defaultModel : Model
defaultModel = Unitialized


-- * message


type Msg
  = RequestCollectionFetched BuilderApp.RequestCollection
  | EnvironmentsFetched (List Environment)
  | ServerError
  | InitializedApplicationMsg InitializedApplication.Msg


-- * init


init : () -> (Model, Cmd Msg)
init _ =
    let
        getRequestCollection =
            Client.getRequestCollectionByRequestCollectionId "" 1 requestCollectionResultToMsg

        getEnvironments =
            Client.getEnvironment "" environmentsResultToMsg

        getInitialState = Cmd.batch [getRequestCollection, getEnvironments]

    in
        (defaultModel, getInitialState)

requestCollectionResultToMsg : Result Http.Error Client.RequestCollection -> Msg
requestCollectionResultToMsg result =
    case result of
        Ok requestCollection ->
            let
                newRequestCollection =
                    Client.convertRequestCollectionFromBackToFront requestCollection
            in
                RequestCollectionFetched newRequestCollection

        Err error ->
            Debug.log "test" ServerError


environmentsResultToMsg : Result Http.Error (List Client.Environment) -> Msg
environmentsResultToMsg result =
    case result of
        Ok clientEnvironments ->
            let
                environments = List.map Client.convertEnvironmentFromBackToFront clientEnvironments
            in
                EnvironmentsFetched environments

        Err error ->
            Debug.log "test" ServerError

upgradeModel : Model -> Model
upgradeModel model =
    case model of
        Pending { mRequestCollection, mEnvironments } ->
            case (mRequestCollection, mEnvironments) of
                (Just requestCollection, Just environments) ->
                    let
                        newModel =
                            Initialized <|
                                InitializedApplication.createModel requestCollection environments
                    in
                        newModel

                _ ->
                    model

        _ ->
            model


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EnvironmentsFetched environments ->
            let
                newModel =
                    case model of
                        Unitialized ->
                            Pending { mEnvironments = Just environments
                                    , mRequestCollection = Nothing
                                    }

                        Pending pending ->
                            Pending { pending | mEnvironments = Just environments } |> upgradeModel

                        Initialized _ ->
                            Debug.todo "already initialized app received initialization infos"
            in
                (newModel, Cmd.none)

        RequestCollectionFetched requestCollection ->
            let
                newModel =
                    case model of
                        Unitialized ->
                            Pending { mEnvironments = Nothing
                                    , mRequestCollection = Just requestCollection
                                    }

                        Pending pending ->
                            Pending { pending | mRequestCollection = Just requestCollection } |> upgradeModel

                        Initialized _ ->
                            Debug.todo "already initialized app received initialization infos"

            in
                (newModel, Cmd.none)

        ServerError ->
            (model, Cmd.none)

        InitializedApplicationMsg subMsg ->
            case model of
                Pending _ ->
                    Debug.todo "InitializedApplicationMsg received with unitialized Application - This should never happen"

                Unitialized ->
                    Debug.todo "InitializedApplicationMsg received with unitialized Application - This should never happen"

                Initialized initializedApplication ->
                    let
                        (newInitializedApplication, newMsg) =
                            InitializedApplication.update subMsg initializedApplication
                    in
                        ( Initialized newInitializedApplication
                        , Cmd.map InitializedApplicationMsg newMsg
                        )


-- * view


view : Model -> Html.Html Msg
view model =
    let
        loadingView =
            el [ width fill
               , height fill
               , Background.color <| secondaryColor
               ]
            <| el [ centerX
                  , centerY
                  , Font.center
                  ]
                <| iconWithText "autorenew" "loading ApiTester..."

    in
        layout [] <|
            case model of
                Unitialized ->
                    loadingView

                Pending _ ->
                    loadingView

                Initialized initializedApplication ->
                    el [ width fill ] <|
                        map InitializedApplicationMsg (html <| InitializedApplication.view initializedApplication)


-- * subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
