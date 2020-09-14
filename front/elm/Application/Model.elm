module Application.Model exposing (..)

import Animation
import Application.Type exposing (..)
import Browser.Navigation as Navigation
import List.Extra as List
import Modal exposing (Modal)
import Page exposing (..)
import Url
import Uuid exposing (Uuid)


-- * model


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    , url : Url.Url
    , session : Session
    , loadingAnimation : Animation.State -- use to fade in the app on init

    -- NOTIFICATION
    , notification : Maybe Notification
    , notificationAnimation : Animation.State

    -- DOCUMENTATION
    , displayedDocumentation : Documentation

    -- Landing page DEMO
    , sceneToDemo : SceneToDemo

    -- MODAL
    , whichModal : Maybe Modal

    -- MENU
    , showMainMenuName : Maybe MainMenuName

    -- BUILDER APP
    , displayedRequestNodeMenuId : Maybe Uuid
    , displayedRequestBuilderView : BuilderView Uuid
    , requestCollection : RequestCollection
    , requestNewNode : NewNode

    -- SQL
    , displayedPgNodeMenuId : Maybe Uuid
    , displayedPgBuilderView : BuilderView Uuid
    , displayedPgId : Maybe Uuid
    , pgCollection : PgCollection
    , pgNewNode : NewNode
    , sqlQuery : Editable String
    , pgComputation : Maybe PgComputation

    -- SCENARIO APP
    , scenarioCollection : ScenarioCollection
    , displayedScenarioNodeMenuId : Maybe Uuid
    , displayedScenarioBuilderView : RichBuilderView Uuid (Maybe Uuid)
    , displayedScenarioId : Maybe Uuid
    , scenarioNewNode : NewNode
    , displayedSceneId : Maybe Uuid

    -- TANGOSCRIPT APP
    , script : String

    -- ENVIRONMENT
    , selectedEnvironmentToRunIndex : Maybe Uuid
    , selectedEnvironmentToEditId : Maybe Uuid
    , displayedEnvId : Maybe Uuid
    , displayedEnvironmentBuilderView : BuilderView Uuid
    , displayedEnvironmentNodeMenuId : Maybe Uuid
    , newEnvironmentName : String
    , environments : List Environment

    -- RUNNER
    , runnerRunning : Bool
    }


-- * scene to demo


type SceneToDemo
    = Scene1
    | Scene2
    | Scene3


-- * environment


type alias GetEnvironment a =
    { a
        | environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Uuid
    }


getEnvironmentToRun : GetEnvironment a -> Maybe Environment
getEnvironmentToRun model =
    let
        selectEnvironment : Uuid -> Maybe Environment
        selectEnvironment id =
            model.environments
                |> List.filter (\env -> env.id == id)
                |> List.head
    in
    Maybe.andThen selectEnvironment model.selectedEnvironmentToRunIndex


getEnvironmentKeyValuesToRun : GetEnvironment a -> List KeyValue
getEnvironmentKeyValuesToRun model =
    getEnvironmentToRun model
        |> Maybe.map .keyValues
        |> Maybe.withDefault []


getEnvironmentToEdit : Model -> Maybe Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Uuid -> Maybe Environment
        selectEnvironment id =
            List.find (\env -> env.id == id) model.environments
    in
    Maybe.andThen selectEnvironment model.selectedEnvironmentToEditId
