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


-- * session


-- ** model


type Model
    = SessionCheckingPending
    | Unsigned
    | Signed SignedAppModel

defaultModel : Model
defaultModel = SessionCheckingPending

type alias Account  =
   { id: Int
   , email: String
   }


-- ** message


type Msg
    = AccountFetched Account
    | AccountNotFetched
    | AskSignOff
    | SignOff
    | AskSignOn
    | SignOn
    | SignedInAppMsg SignedAppMsg
    | ServerError


-- ** init


init : () -> (Model, Cmd Msg)
init _ =
    let
        msg =
            Client.getAccountMe "" getAccountResultToMsg
    in
        (defaultModel, msg)


getAccountResultToMsg : Result Http.Error Client.Account -> Msg
getAccountResultToMsg result =
    case result of
        Ok account ->
            let
                newAccount =
                    Client.convertAccountFromBackToFront account
            in
                AccountFetched newAccount

        Err (Http.BadStatus 401) ->
             AccountNotFetched

        Err error ->
            Debug.log "test" ServerError


-- ** update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AccountFetched _ ->
            let
                newModel =
                    Signed Unitialized
            in
                (newModel, Cmd.none)--Cmd.map SignedAppMsg init)

        AccountNotFetched ->
            (Unsigned, Cmd.none)

        _ ->
            let
                newModel = Unsigned
            in
                (newModel, Cmd.none)


-- ** view

view : Model -> Html.Html Msg
view model =
    layout [] <|
        case model of
            SessionCheckingPending ->
                loadingView

            Unsigned ->
                unsignedView

            Signed signedAppModel ->
                loadingView


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


-- * signed in app


-- ** model


type SignedAppModel
    = Unitialized
    | Pending
      { mRequestCollection : Maybe BuilderApp.RequestCollection
      , mEnvironments : Maybe (List Environment)
      }
    | Initialized InitializedApplication.Model


-- ** message


type SignedAppMsg
  = RequestCollectionFetched BuilderApp.RequestCollection
  | EnvironmentsFetched (List Environment)
  | SignedAppServerError
  | InitializedApplicationMsg InitializedApplication.Msg


-- ** init


initSignedApp : Cmd SignedAppMsg
initSignedApp =
    let
        getRequestCollection =
            Client.getRequestCollectionByRequestCollectionId "" 1 requestCollectionResultToMsg

        getEnvironments =
            Client.getEnvironment "" environmentsResultToMsg

        getInitialState = Cmd.batch [getRequestCollection, getEnvironments]

    in
        getInitialState

requestCollectionResultToMsg : Result Http.Error Client.RequestCollection -> SignedAppMsg
requestCollectionResultToMsg result =
    case result of
        Ok requestCollection ->
            let
                newRequestCollection =
                    Client.convertRequestCollectionFromBackToFront requestCollection
            in
                RequestCollectionFetched newRequestCollection

        Err error ->
            Debug.log "test" SignedAppServerError


environmentsResultToMsg : Result Http.Error (List Client.Environment) -> SignedAppMsg
environmentsResultToMsg result =
    case result of
        Ok clientEnvironments ->
            let
                environments = List.map Client.convertEnvironmentFromBackToFront clientEnvironments
            in
                EnvironmentsFetched environments

        Err error ->
            Debug.log "test" SignedAppServerError

upgradeModel : SignedAppModel -> SignedAppModel
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


-- ** update


updateSignedApp : SignedAppMsg -> SignedAppModel -> (SignedAppModel, Cmd SignedAppMsg)
updateSignedApp msg model =
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

        SignedAppServerError ->
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


-- ** view


signedAppView : SignedAppModel -> Html.Html SignedAppMsg
signedAppView model =
    layout [] <|
        case model of
            Unitialized ->
                loadingView

            Pending _ ->
                loadingView

            Initialized initializedApplication ->
                el [ width fill ] <|
                    map InitializedApplicationMsg (InitializedApplication.view initializedApplication)


-- ** subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
