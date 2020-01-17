module InitializedApplication.Model exposing (..)

import VarApp.Model as VarApp
import BuilderApp.Model as BuilderApp
import List.Extra as List
import Application.Type exposing (..)
import Page exposing(..)

type alias Model =
    { session : Session
    , page : Page
    -- INITIALIZE PASSWORD
    , initializePassword1 : String
    , initializePassword2 : String
    , initialiazePasswordError : Maybe String
    -- BUILDER APP
    , selectedBuilderIndex : Maybe Int
    , displayedBuilderIndex : Maybe Int
    , displayedRequestNodeMenuIndex : Maybe Int
    , requestCollection : BuilderApp.RequestCollection
    -- POSTMAN
    , postmanModel : Maybe (List BuilderApp.RequestNode)
    -- ENVIRONMENT
    , selectedEnvironmentToRunIndex : Maybe Int
    , selectedEnvironmentToEditId : Maybe Int
    , environments : List Environment
    -- VARIABLE APP
    , varAppModel : VarApp.Model
    }


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

createModel : Page -> Session -> BuilderApp.RequestCollection -> List Environment -> Model
createModel page session requestCollection environments =
  let
      selectedBuilderIndex = Nothing
      displayedBuilderIndex = Nothing
      displayedRequestNodeMenuIndex = Nothing
      varAppModel =
          { vars = []
          , overZoneId = Nothing
          , draggedId = Nothing
          }
      selectedEnvironmentToEditId = Just 0
      selectedEnvironmentToRunIndex = Just 0
  in
      { session = session
      , page = Debug.log "page" page
      , initializePassword1 = ""
      , initializePassword2 = ""
      , initialiazePasswordError = Nothing
      , selectedBuilderIndex = selectedBuilderIndex
      , displayedBuilderIndex = displayedBuilderIndex
      , displayedRequestNodeMenuIndex = displayedRequestNodeMenuIndex
      , requestCollection = requestCollection
      , postmanModel = Nothing
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditId = selectedEnvironmentToEditId
      , environments = environments
      , varAppModel = varAppModel
      }
