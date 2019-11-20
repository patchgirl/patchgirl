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
    , selectedEnvironmentToEditIndex : Maybe Int
    , selectedEnvironmentToRenameIndex : Maybe Int
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

getEnvironmentKeyValuesToRun : GetEnvironment a -> List(String, String)
getEnvironmentKeyValuesToRun model =
    (getEnvironmentToRun model)
        |> Maybe.map (.keyValues)
        |> Maybe.map (editedOrNotEditedValue)
        |> Maybe.withDefault []

getEnvironmentKeyValuesToEdit : Model -> List(String, String)
getEnvironmentKeyValuesToEdit model =
    EnvironmentEdition.getEnvironmentToEdit model
        |> Maybe.map .keyValues
        |> Maybe.map (editedOrNotEditedValue)
        |> Maybe.withDefault []

createModel : BuilderApp.RequestCollection -> Model
createModel requestCollection =
  let
      selectedBuilderIndex = Nothing
      displayedBuilderIndex = Nothing
      displayedRequestNodeMenuIndex = Nothing
      varAppModel =
          { vars =
                [ ("key0", "value0")
                , ("key1", "value1")
                , ("key2", "value2")
                , ("key3", "value3")
                , ("key4", "value4")
                ]
          , overZoneId = Nothing
          , draggedId = Nothing
          }
      selectedEnvironmentToEditIndex = Just 0
      selectedEnvironmentToRunIndex = Just 0
      selectedEnvironmentToRenameIndex = Nothing
      environments =
          [ { name = "staging1"
            , keyValues =
                  NotEdited [ ("key1", "value1")
                            , ("key2", "value2")
                            ]
            }
          , { name = "staging2"
            , keyValues =
                  NotEdited [ ("key3", "value3")
                            ]
            }
          ]
  in
      { mainNavBarModel = MainNavBar.defaultModel
      , selectedBuilderIndex = selectedBuilderIndex
      , displayedBuilderIndex = displayedBuilderIndex
      , displayedRequestNodeMenuIndex = displayedRequestNodeMenuIndex
      , requestCollection = requestCollection
      , postmanModel = Nothing
      , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
      , selectedEnvironmentToEditIndex = selectedEnvironmentToEditIndex
      , selectedEnvironmentToRenameIndex = selectedEnvironmentToRenameIndex
      , environments = environments
      , runnerModel = Nothing
      , varAppModel = varAppModel
      }
