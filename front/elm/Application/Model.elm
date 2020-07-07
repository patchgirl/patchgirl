module Application.Model exposing (..)

import Animation
import Application.Type exposing (..)
import Browser.Navigation as Navigation
import List.Extra as List
import Modal exposing (Modal)
import Page exposing (..)
import Url as Url
import Uuid
import Api.RunnerGeneratedClient as Client


-- * model


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    , url : Url.Url
    , session : Session
    , loadingAnimation : Animation.State -- use to fade in the app on init

    -- NOTIFICATION
    , notification : Maybe String
    , notificationAnimation : Animation.State

    -- MODAL
    , whichModal : Maybe Modal

    -- MENU
    , showMainMenuName : Maybe MainMenuName

    -- BUILDER APP
    , displayedRequestNodeMenuId : Maybe Uuid.Uuid
    , requestCollection : RequestCollection

    -- SQL
    , pgCollection : PgCollection
    , sqlQuery : Editable String
    , pgComputation : Client.PGComputation

    -- SCENARIO APP
    , scenarioCollection : ScenarioCollection
    , displayedScenarioNodeMenuId : Maybe Uuid.Uuid

    -- TANGOSCRIPT APP
    , script : String

    -- ENVIRONMENT
    , selectedEnvironmentToRunIndex : Maybe Int
    , selectedEnvironmentToEditId : Maybe Int
    , environments : List Environment

    -- RUNNER
    , runnerRunning : Bool
    }



-- * environment


type alias GetEnvironment a =
    { a
        | environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
    }


getEnvironmentToRun : GetEnvironment a -> Maybe Environment
getEnvironmentToRun model =
    let
        selectEnvironment : Int -> Maybe Environment
        selectEnvironment idx =
            List.getAt idx model.environments
    in
    Maybe.andThen selectEnvironment model.selectedEnvironmentToRunIndex


getEnvironmentKeyValuesToRun : GetEnvironment a -> List (Storable NewKeyValue KeyValue)
getEnvironmentKeyValuesToRun model =
    getEnvironmentToRun model
        |> Maybe.map .keyValues
        |> Maybe.withDefault []


getEnvironmentToEdit : Model -> Maybe Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Int -> Maybe Environment
        selectEnvironment id =
            List.find (\env -> env.id == id) model.environments
    in
    Maybe.andThen selectEnvironment model.selectedEnvironmentToEditId
