module EnvironmentEdition.App exposing (..)

import List.Extra as List

import EnvironmentEdition.Message exposing (Msg(..))

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition
import EnvironmentEdition.Model exposing (..)
import EnvironmentEdition.Util exposing (..)
import Application.Type exposing (..)
import Api.Client as Client
import Http as Http

defaultEnvironment =
    { id = 0
    , name = NotEdited "new environment"
    , showRenameInput = False
    , keyValues = NotEdited []
    }

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
  case msg of
    SelectEnvToEdit id ->
      let
          newModel =
              { model | selectedEnvironmentToEditId = Just id }
      in
          (newModel, Cmd.none)

    AskEnvironmentCreation name ->
        let
            payload =
                { name = name
                }

            newMsg =
                Client.postEnvironment "" payload (newEnvironmentResultToMsg name)
        in
            (model, newMsg)

    EnvironmentCreated id name ->
        let
            newEnvironment =
                { id = id
                , name = NotEdited name
                , showRenameInput = True
                , keyValues = NotEdited []
                }

            newEnvironments =
                model.environments ++ [ newEnvironment ]

            newModel =
                { model | environments = newEnvironments }
        in
            (newModel, Cmd.none)

    ServerError ->
        Debug.todo "server error :-("

    ShowRenameInput id ->
        let
            updateEnv old =
                { old | showRenameInput = True }

            newEnvironments =
                List.updateIf (\elem -> elem.id == id) updateEnv model.environments

            newModel =
                { model | environments = newEnvironments }
        in
            (newModel, Cmd.none)

    AskRename id name ->
        let
            payload =
                { name = name
                }

            newMsg =
                Client.putEnvironmentByEnvironmentId "" id payload (updateEnvironmentResultToMsg id name)
        in
            (model, newMsg)

    EnvironmentRenamed id name ->
        let
            updateEnv old =
                { old
                    | name = NotEdited name
                    , showRenameInput = False
                }

            mNewEnvs =
                List.updateIf (\elem -> elem.id == id) updateEnv model.environments

            newModel =
                { model
                    | environments = mNewEnvs
                }
        in
            (newModel, Cmd.none)

    AskDelete id ->
        let
            newMsg =
                Client.deleteEnvironmentByEnvironmentId "" id (deleteEnvironmentResultToMsg id)
        in
            (model, newMsg)

    EnvironmentDeleted id ->
        let
            newEnvironments =
                List.filter (\elem -> elem.id /= id) model.environments

            newSelectedEnvironmentToEditId =
                case model.selectedEnvironmentToEditId == Just id of
                    True -> Nothing
                    False -> model.selectedEnvironmentToEditId

            newModel =
                { model
                    | selectedEnvironmentToEditId = newSelectedEnvironmentToEditId
                    , environments = newEnvironments
                }

        in
            (newModel, Cmd.none)

    ChangeName id name ->
        let
            updateEnv old =
                let
                    newName =
                        changeEditedValue name old.name
                in
                    { old | name = newName }

            mNewEnvs =
                List.updateIf (\elem -> elem.id == id) updateEnv model.environments

            newModel =
                { model
                    | environments = mNewEnvs
                }
      in
          (newModel, Cmd.none)


    EnvironmentKeyValueEditionMsg subMsg ->
        case getEnvironmentToEdit model of
            Nothing ->
                (model, Cmd.none)

            Just environment ->
                case EnvironmentKeyValueEdition.update subMsg environment of
                    newEnvironment ->
                        let
                            -- todo fix 0 -> should be model.selectedEnvironmentToEditId
                            newEnvironments = List.setAt 0 newEnvironment model.environments

                            newModel =
                                { model | environments = newEnvironments }

                        in
                            (newModel, Cmd.none)

newEnvironmentResultToMsg : String -> Result Http.Error Int -> Msg
newEnvironmentResultToMsg name result =
    case result of
        Ok id ->
            EnvironmentCreated id name

        Err error ->
            Debug.log "test" ServerError

updateEnvironmentResultToMsg : Int -> String -> Result Http.Error () -> Msg
updateEnvironmentResultToMsg id name result =
    case result of
        Ok () ->
            EnvironmentRenamed id name

        Err error ->
            Debug.todo "server error" ServerError

deleteEnvironmentResultToMsg : Int -> Result Http.Error () -> Msg
deleteEnvironmentResultToMsg id result =
    case result of
        Ok () ->
            EnvironmentDeleted id

        Err error ->
            Debug.todo "server error" ServerError
