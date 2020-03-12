module Application.Model exposing (..)

import List.Extra as List
import Application.Type exposing (..)
import Page exposing(..)
import Uuid
import Browser.Navigation as Navigation
import Animation
import Url as Url


-- * model


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    , url : Url.Url
    , session : Session
    , loadingStyle : Animation.State -- use to fade in the app on init
    -- MENU
    , showMainMenuName : Maybe MainMenuName
    -- INITIALIZE PASSWORD
    , initializePassword1 : String
    , initializePassword2 : String
    , initializePasswordState : InitializePasswordState
    -- BUILDER APP
    , displayedRequestNodeMenuId : Maybe Uuid.Uuid
    , requestCollection : RequestCollection
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
