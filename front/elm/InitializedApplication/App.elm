module InitializedApplication.App exposing (..)

import InitializedApplication.Model exposing (..)
import InitializedApplication.Message exposing (..)
import InitializedApplication.Util exposing (..)

import BuilderApp.App as BuilderApp
import BuilderApp.Model as BuilderApp
import BuilderApp.Message as BuilderApp
import BuilderApp.App as BuilderApp
import BuilderApp.Message as BuilderApp

import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Model as Builder
import BuilderApp.Builder.Message as Builder
import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Message as Builder

import BuilderApp.BuilderTree.View as BuilderTree
import BuilderApp.BuilderTree.Message as BuilderTree
import BuilderApp.BuilderTree.App as BuilderTree

import Postman.View as Postman
import Postman.Model as Postman
import Postman.Message as Postman
import Postman.App as Postman

import EnvironmentKeyValueEdition.View as EnvironmentKeyValueEdition
import EnvironmentKeyValueEdition.Message as EnvironmentKeyValueEdition
import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition

import EnvironmentEdition.View as EnvironmentEdition
import EnvironmentEdition.Message as EnvironmentEdition
import EnvironmentEdition.App as EnvironmentEdition
import EnvironmentEdition.Util as EnvironmentEdition

import EnvironmentToRunSelection.Message as EnvSelection
import EnvironmentToRunSelection.App as EnvSelection

import RequestRunner.App as RequestRunner
import RequestRunner.Message as RequestRunner
import RequestRunner.Model as RequestRunner
import RequestRunner.Util as RequestRunner

import MainNavBar.Model as MainNavBar
import MainNavBar.View as MainNavBar
import MainNavBar.Message as MainNavBar
import MainNavBar.App as MainNavBar

import VarApp.Model as VarApp
import VarApp.View as VarApp
import VarApp.Message as VarApp
import VarApp.App as VarApp

import Api.Client as Client

import Util.Flip exposing (..)
import Util.List as List
import List.Extra as List

import Curl.Util as Curl
import Http as Http

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        BuilderTreeMsg subMsg ->
            let
                newModel = BuilderTree.update subMsg model
            in
                (newModel, Cmd.none)

        BuilderAppMsg subMsg ->
            case subMsg of
                {-
                BuilderApp.BuilderMsg Builder.AskRun ->
                    (model, Cmd.none)-}
--                    case getEnvironmentToRun model of
                        {-
                        Just environment ->
                            case RequestRunner.update (RequestRunner.Run environment.keyValues model.varAppModel builder) Nothing of
                              (_, runnerSubMsg) -> (model, Cmd.map RequestRunnerMsg runnerSubMsg)

                        Nothing ->-}


                BuilderApp.BuilderMsg (Builder.ShowRequestAsCurl) ->
                    (model, Cmd.none)

                _ ->
                    let
                        (newModel, newMsg) = BuilderApp.update subMsg model
                    in
                        (newModel, Cmd.map BuilderAppMsg newMsg)

        EnvironmentEditionMsg subMsg ->
            case EnvironmentEdition.update subMsg model of
                newModel ->
                    (newModel, Cmd.none)

        PostmanMsg subMsg ->
            case Postman.update subMsg model.postmanModel of
                (_, _) -> (model, Cmd.none)


        RequestRunnerMsg subMsg ->
            (model, Cmd.none)

        EnvironmentKeyValueEditionMsg subMsg ->
            case EnvironmentEdition.getEnvironmentToEdit model of
                Just environment ->
                    case EnvironmentKeyValueEdition.update subMsg environment of
                        newEnvironmentKeyValueEdition ->
                            let
                                newModel = replaceEnvironmentToEdit model newEnvironmentKeyValueEdition
                            in
                                (newModel, Cmd.none)

                Nothing ->
                    (model, Cmd.none)

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model.mainNavBarModel of
                newMainNavBarModel ->
                    ( { model | mainNavBarModel = newMainNavBarModel }
                    , Cmd.none
                    )

        VarAppMsg subMsg ->
            case VarApp.update subMsg model.varAppModel of
                newVarAppModel ->
                    ( { model | varAppModel = newVarAppModel }
                    , Cmd.none
                    )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
