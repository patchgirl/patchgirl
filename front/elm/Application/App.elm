module Application.App exposing (..)

import Http as Http
import Api.Client as Client
import Api.Converter as Client
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Html as Html

import InitializedApplication.Model as InitializedApplication
import InitializedApplication.App as InitializedApplication
import BuilderApp.Model as BuilderApp
import Application.Type exposing (..)


-- * session


-- ** model


type Model
    = SessionPending
    | AppDataPending
      { session : Session
      , mRequestCollection : Maybe BuilderApp.RequestCollection
      , mEnvironments : Maybe (List Environment)
      }
    | InitializedApp InitializedApplication.Model

defaultModel : Model
defaultModel = SessionPending


-- ** message


type Msg
    = SessionFetched Session
    | RequestCollectionFetched BuilderApp.RequestCollection
    | EnvironmentsFetched (List Environment)
    | InitializedApplicationMsg InitializedApplication.Msg
    | ServerError Http.Error


-- ** init


init : () -> (Model, Cmd Msg)
init _ =
    let
        msg =
            Client.getSessionWhoami "" "" getSessionWhoamiResult
    in
        (defaultModel, msg)


getSessionWhoamiResult : Result Http.Error Client.Session -> Msg
getSessionWhoamiResult result =
    case result of
        Ok session ->
            let
                newSession =
                    Client.convertSessionFromBackToFront session
            in
                SessionFetched newSession

        Err error ->
            Debug.log "whoami" (ServerError error)


-- ** update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SessionFetched session ->
            let
                newModel =
                    AppDataPending { session = session
                                   , mRequestCollection = Nothing
                                   , mEnvironments = Nothing
                                   }

                getRequestCollection =
                    Client.getRequestCollectionByRequestCollectionId "" (getCsrfToken session) (getSessionId session) requestCollectionResultToMsg

                getEnvironments =
                    Client.getEnvironment "" (getCsrfToken session) environmentsResultToMsg

                getAppData =
                    Cmd.batch
                        [ getRequestCollection
                        , getEnvironments
                        ]

            in
                (newModel, getAppData)

        EnvironmentsFetched environments ->
            case model of
                AppDataPending pending ->
                    AppDataPending { pending | mEnvironments = Just environments }
                        |> upgradeModel

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        RequestCollectionFetched requestCollection ->
            case model of
                AppDataPending pending ->
                    AppDataPending { pending | mRequestCollection = Just requestCollection }
                        |> upgradeModel

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        InitializedApplicationMsg subMsg ->
            case model of
                InitializedApp initializedApplication ->
                    let
                        (newInitializedApplication, newMsg) =
                            InitializedApplication.update subMsg initializedApplication
                    in
                        ( InitializedApp newInitializedApplication
                        , Cmd.map InitializedApplicationMsg newMsg
                        )

                _ ->
                    Debug.todo "InitializedApplicationMsg received with unitialized Application - This should never happen"


        ServerError error ->
            Debug.todo "server error" error

-- ** util

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
            ServerError error



environmentsResultToMsg : Result Http.Error (List Client.Environment) -> Msg
environmentsResultToMsg result =
    case result of
        Ok clientEnvironments ->
            let
                environments = List.map Client.convertEnvironmentFromBackToFront clientEnvironments
            in
                EnvironmentsFetched environments

        Err error ->
            ServerError error


upgradeModel : Model -> (Model, Cmd Msg)
upgradeModel model =
    case model of
        AppDataPending { session, mRequestCollection, mEnvironments } ->
            case (mRequestCollection, mEnvironments) of
                (Just requestCollection, Just environments) ->
                    let
                        newModel =
                            InitializedApp <|
                                InitializedApplication.createModel session requestCollection environments
                    in
                        (newModel, Cmd.none)

                _ ->
                    (model, Cmd.none)

        _ ->
            (model, Cmd.none)


-- ** view


view : Model -> Html.Html Msg
view model =
    layout [] <|
        case model of
            SessionPending ->
                loadingView

            AppDataPending _ ->
                loadingView

            InitializedApp initializedApplication ->
                el [ width fill ] <|
                    map InitializedApplicationMsg (InitializedApplication.view initializedApplication)


unsignedView : Element Msg
unsignedView = none

loadingView : Element a
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



-- ** subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
