module InitializedApplication.Model exposing (..)

import MainNavBar.Model as MainNavBar
import RequestRunner.Model as RequestRunner
import EnvironmentEdition.Util as EnvironmentEdition
import VarApp.Model as VarApp
import BuilderApp.Model as BuilderApp
import List.Extra as List
import Application.Type exposing (..)

type alias Model =
    { mainNavBarModel : MainNavBar.Model
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
    , selectedEnvironmentToRenameId : Maybe Int
    , environments : List Environment
    -- VARIABLE APP
    , varAppModel : VarApp.Model
    -- RUNNER
    , runnerModel : RequestRunner.Model
    }

{-type alias Model2
    = Home Home.Model
    | Request Request.Model-}

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

getEnvironmentKeyValuesToRun : GetEnvironment a -> List KeyValue
getEnvironmentKeyValuesToRun model =
    (getEnvironmentToRun model)
        |> Maybe.map (.keyValues)
        |> Maybe.map (editedOrNotEditedValue)
        |> Maybe.withDefault []

getEnvironmentKeyValuesToEdit : Model -> List KeyValue
getEnvironmentKeyValuesToEdit model =
    EnvironmentEdition.getEnvironmentToEdit model
        |> Maybe.map .keyValues
        |> Maybe.map (editedOrNotEditedValue)
        |> Maybe.withDefault []

createModel : BuilderApp.RequestCollection -> List Environment -> Model
createModel requestCollection environments =
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
      selectedEnvironmentToRenameId = Nothing
  in
      { mainNavBarModel = MainNavBar.defaultModel
      , selectedBuilderIndex = selectedBuilderIndex
      , displayedBuilderIndex = displayedBuilderIndex
      , displayedRequestNodeMenuIndex = displayedRequestNodeMenuIndex
      , requestCollection = requestCollection
      , postmanModel = Nothing
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditId = selectedEnvironmentToEditId
      , selectedEnvironmentToRenameId = selectedEnvironmentToRenameId
      , environments = environments
      , runnerModel = Nothing
      , varAppModel = varAppModel
      }
