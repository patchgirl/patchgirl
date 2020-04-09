module Application.Model exposing (..)

import List.Extra as List
import Application.Type exposing (..)
import Page exposing(..)
import Uuid
import Browser.Navigation as Navigation
import Animation
import Url as Url
import Modal exposing(..)


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
    -- SCENARIO APP
    , scenarioCollection : ScenarioCollection
    , displayedScenarioNodeMenuId : Maybe Uuid.Uuid
    -- ENVIRONMENT
    , selectedEnvironmentToRunIndex : Maybe Int
    , selectedEnvironmentToEditId : Maybe Int
    , environments : List Environment
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
        selectEnvironment idx = List.getAt idx model.environments
    in
        Maybe.andThen selectEnvironment model.selectedEnvironmentToRunIndex

getEnvironmentKeyValuesToRun : GetEnvironment a -> List (Storable NewKeyValue KeyValue)
getEnvironmentKeyValuesToRun model =
    (getEnvironmentToRun model)
        |> Maybe.map (.keyValues)
        |> Maybe.withDefault []

getEnvironmentToEdit : Model -> Maybe Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Int -> Maybe Environment
        selectEnvironment id = List.find (\env -> env.id == id) model.environments
    in
        Maybe.andThen selectEnvironment model.selectedEnvironmentToEditId
