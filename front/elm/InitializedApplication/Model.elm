module InitializedApplication.Model exposing (..)

import List.Extra as List
import Application.Type exposing (..)
import Page exposing(..)
import Uuid

type alias Model =
    { session : Session
    , page : Page
    -- INITIALIZE PASSWORD
    , initializePassword1 : String
    , initializePassword2 : String
    , initializePasswordState : InitializePasswordState
    -- BUILDER APP
    , selectedBuilderId : Maybe Uuid.Uuid
    , displayedBuilderIndex : Maybe Int
    , displayedRequestNodeMenuId : Maybe Uuid.Uuid
    , requestCollection : RequestCollection
    -- POSTMAN
    --, postmanModel : Maybe (List BuilderApp.RequestNode)
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

getEnvironmentKeyValuesToEdit : Model -> List (Storable NewKeyValue KeyValue)
getEnvironmentKeyValuesToEdit model =
    getEnvironmentToEdit model
        |> Maybe.map .keyValues
        |> Maybe.withDefault []


-- * model


createModel : Page -> Session -> RequestCollection -> List Environment -> Model
createModel page session requestCollection environments =
  let
      selectedBuilderId = Nothing
      displayedBuilderIndex = Nothing
      displayedRequestNodeMenuId = Nothing
      selectedEnvironmentToEditId = Just 0
      selectedEnvironmentToRunIndex = Just 0
  in
      { session = session
      , page = page
      , initializePassword1 = ""
      , initializePassword2 = ""
      , initializePasswordState = InitialPasswordState
      , selectedBuilderId = selectedBuilderId
      , displayedBuilderIndex = displayedBuilderIndex
      , displayedRequestNodeMenuId = displayedRequestNodeMenuId
      , requestCollection = requestCollection
      --, postmanModel = Nothing
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditId = selectedEnvironmentToEditId
      , environments = environments
      }
