module Page exposing (Page(..), href, urlToPage, Documentation(..), documentationToString, documentationFromString)

import Url
import Url.Parser as Url exposing ((</>))
import Uuid exposing (Uuid)
import Application.Type exposing (..)


-- * model

type Page
    = HomePage
    | ReqPage (BuilderView Uuid)
    | PgPage (BuilderView Uuid)
    | EnvPage (BuilderView Uuid)
    | ScenarioPage (RichBuilderView Uuid (Maybe Uuid))
    | NotFoundPage
    | DocumentationPage Documentation
    | TangoScriptPage

type Documentation
    = ScenarioDoc
    | RequestDoc
    | PostgresDoc
    | EnvironmentDoc
    | PatchGirlRunnerAppDoc
    | TangoscriptDoc

documentationToString : Documentation -> String
documentationToString documentation =
    case documentation of
        ScenarioDoc ->
            "scenario"

        RequestDoc ->
            "http-request"

        PostgresDoc ->
            "postgres"

        EnvironmentDoc ->
            "environment-variables"

        PatchGirlRunnerAppDoc ->
            "patchgirl-runner"

        TangoscriptDoc ->
            "tangoscript"

documentationFromString : String -> Maybe Documentation
documentationFromString documentation =
    case documentation of
        "scenario" -> Just ScenarioDoc
        "http-request" -> Just RequestDoc
        "postgres" -> Just PostgresDoc
        "environment-variables" -> Just EnvironmentDoc
        "patchgirl-runner" -> Just PatchGirlRunnerAppDoc
        "tangoscript" -> Just TangoscriptDoc
        _ -> Nothing


-- * parser


uuidParser : Url.Parser (Uuid -> b) b
uuidParser =
    Url.custom "UUID" Uuid.fromString

documentationParser : Url.Parser (Documentation -> b) b
documentationParser =
    let
        parser : String -> Maybe Documentation
        parser s =
            case s of
                "http-request" ->
                    Just RequestDoc

                "postgres" ->
                    Just PostgresDoc

                "scenario" ->
                    Just ScenarioDoc

                "environment-variables" ->
                    Just EnvironmentDoc

                "patchgirl-runner" ->
                    Just PatchGirlRunnerAppDoc

                "tangoscript" ->
                    Just TangoscriptDoc

                _  ->
                    Nothing
    in
    Url.custom "Documentation" parser


urlParser : Url.Parser (Page -> a) a
urlParser =
    let
        appRoot = Url.s "app"
    in
    Url.oneOf
        [ Url.map HomePage Url.top
        -- http
        , Url.map (ReqPage (LandingView DefaultView)) (appRoot </> Url.s "req")
        , Url.map (ReqPage (LandingView CreateDefaultFolderView)) (appRoot </> Url.s "req" </> Url.s "new-folder")
        , Url.map (ReqPage (LandingView CreateDefaultFileView)) (appRoot </> Url.s "req" </> Url.s "new-file")
        , Url.map (\reqId -> ReqPage (EditView (DefaultEditView reqId))) (appRoot </> Url.s "req" </> uuidParser </> Url.s "edit")
        , Url.map (\reqId -> ReqPage (EditView (DeleteView reqId))) (appRoot </> Url.s "req" </> uuidParser </> Url.s "edit" </> Url.s "delete")
        , Url.map (\reqId -> ReqPage (EditView (DuplicateView reqId))) (appRoot </> Url.s "req" </> uuidParser </> Url.s "edit" </> Url.s "duplicate")
        , Url.map (\reqId -> ReqPage (RunView reqId)) (appRoot </> Url.s "req" </> Url.s "run" </> uuidParser)

        -- pg
        , Url.map (PgPage (LandingView DefaultView)) (appRoot </> Url.s "pg")
        , Url.map (PgPage (LandingView CreateDefaultFolderView)) (appRoot </> Url.s "pg" </> Url.s "new-folder")
        , Url.map (PgPage (LandingView CreateDefaultFileView)) (appRoot </> Url.s "pg" </> Url.s "new-file")
        , Url.map (\pgId -> PgPage (EditView (DefaultEditView pgId))) (appRoot </> Url.s "pg" </> uuidParser </> Url.s "edit")
        , Url.map (\pgId -> PgPage (EditView (DeleteView pgId))) (appRoot </> Url.s "pg" </> uuidParser </> Url.s "edit" </> Url.s "delete")
        , Url.map (\pgId -> PgPage (RunView pgId)) (appRoot </> Url.s "pg" </> Url.s "run" </> uuidParser)

        -- env
        , Url.map (EnvPage (LandingView DefaultView)) (appRoot </> Url.s "environment")
        , Url.map (EnvPage (LandingView CreateDefaultFolderView)) (appRoot </> Url.s "environment" </> Url.s "new-folder")
        , Url.map (EnvPage (LandingView CreateDefaultFileView)) (appRoot </> Url.s "environment" </> Url.s "new-file")
        , Url.map (\envId -> EnvPage (EditView (DefaultEditView envId))) (appRoot </> Url.s "environment" </> uuidParser </> Url.s "edit")
        , Url.map (\envId -> EnvPage (EditView (DeleteView envId))) (appRoot </> Url.s "environment" </> uuidParser </> Url.s "edit" </> Url.s "delete")
        , Url.map (\envId -> EnvPage (RunView envId)) (appRoot </> Url.s "environment" </> Url.s "run" </> uuidParser)

        -- scenario
        , Url.map (ScenarioPage (RichLandingView DefaultView)) (appRoot </> Url.s "scenario")
        , Url.map (ScenarioPage (RichLandingView CreateDefaultFolderView)) (appRoot </> Url.s "scenario" </> Url.s "new-folder")
        , Url.map (ScenarioPage (RichLandingView CreateDefaultFileView)) (appRoot </> Url.s "scenario" </> Url.s "new-file")
        , Url.map (\envId -> ScenarioPage (RichEditView (DefaultEditView envId))) (appRoot </> Url.s "scenario" </> uuidParser </> Url.s "edit")
        , Url.map (\envId -> ScenarioPage (RichEditView (DeleteView envId))) (appRoot </> Url.s "scenario" </> uuidParser </> Url.s "edit" </> Url.s "delete")
        , Url.map (\envId sceneId -> ScenarioPage (RichRunView envId (Just sceneId))) (appRoot </> Url.s "scenario" </> Url.s "run" </> uuidParser </> uuidParser)
        , Url.map (\envId -> ScenarioPage (RichRunView envId Nothing)) (appRoot </> Url.s "scenario" </> Url.s "run" </> uuidParser)

        -- other
        , Url.map (\documentation -> DocumentationPage documentation) (appRoot </> Url.s "documentation" </> documentationParser)
        , Url.map (TangoScriptPage) (appRoot </> Url.s "tangoscript")
        ]


-- * href


href : Page -> String
href page =
    let
        pieces =
            case page of
                HomePage ->
                    []

                ReqPage builderView ->
                    let
                        mode =
                            case builderView of
                                LandingView DefaultView ->
                                    []

                                LandingView CreateDefaultFolderView ->
                                    [ "new-folder" ]

                                LandingView CreateDefaultFileView ->
                                    [ "new-file" ]

                                EditView (DefaultEditView id) ->
                                    [ Uuid.toString id, "edit" ]

                                EditView (DeleteView id) ->
                                    [ Uuid.toString id, "edit", "delete" ]

                                EditView (DuplicateView id) ->
                                    [ Uuid.toString id, "edit", "duplicate" ]

                                RunView id ->
                                    [ "run", Uuid.toString id ]

                    in
                    [ "app", "req" ] ++ mode

                PgPage builderView ->
                    let
                        mode =
                            case builderView of
                                LandingView DefaultView ->
                                    []

                                LandingView CreateDefaultFolderView ->
                                    [ "new-folder" ]

                                LandingView CreateDefaultFileView ->
                                    [ "new-file" ]

                                EditView (DefaultEditView id) ->
                                    [ Uuid.toString id, "edit" ]

                                EditView (DeleteView id) ->
                                    [ Uuid.toString id, "edit", "delete" ]

                                EditView (DuplicateView id) ->
                                    [ Uuid.toString id, "edit", "duplicate" ]

                                RunView id ->
                                    [ "run", Uuid.toString id ]

                    in
                    [ "app", "pg" ] ++ mode

                EnvPage builderView ->
                    let
                        mode =
                            case builderView of
                                LandingView DefaultView ->
                                    []

                                LandingView CreateDefaultFolderView ->
                                    [ "new-folder" ]

                                LandingView CreateDefaultFileView ->
                                    [ "new-file" ]

                                EditView (DefaultEditView id) ->
                                    [ Uuid.toString id, "edit" ]

                                EditView (DeleteView id) ->
                                    [ Uuid.toString id, "edit", "delete" ]

                                EditView (DuplicateView id) ->
                                    [ Uuid.toString id, "edit", "duplicate" ]

                                RunView id ->
                                    [ "run", Uuid.toString id ]

                    in
                    [ "app", "environment" ] ++ mode

                ScenarioPage builderView  ->
                    let
                        mode =
                            case builderView of
                                RichLandingView DefaultView ->
                                    []

                                RichLandingView CreateDefaultFolderView ->
                                    [ "new-folder" ]

                                RichLandingView CreateDefaultFileView ->
                                    [ "new-file" ]

                                RichEditView (DefaultEditView id) ->
                                    [ Uuid.toString id, "edit" ]

                                RichEditView (DeleteView id) ->
                                    [ Uuid.toString id, "edit", "delete" ]

                                RichEditView (DuplicateView id) ->
                                    [ Uuid.toString id, "edit", "duplicate" ]

                                RichRunView id Nothing ->
                                    [ "run", Uuid.toString id ]

                                RichRunView id (Just sceneId) ->
                                    [ "run", Uuid.toString id, Uuid.toString sceneId ]

                    in
                    [ "app", "scenario" ] ++ mode

                TangoScriptPage ->
                    [ "app", "tangoscript" ]

                DocumentationPage documentation ->
                    [ "app", "documentation", documentationToString documentation ]

                NotFoundPage ->
                    [ "app", "notFound" ]
    in
    "#" ++ String.join "/" pieces



-- * util


urlToPage : Url.Url -> Page
urlToPage url =
    let
        {-
           The RealWorld spec treats the fragment like a path.
           This makes it *literally* the path, so we can proceed
           with parsing as if it had been a normal path all along.
        -}
        urlWithoutFragment =
            { url
                | path = Maybe.withDefault "" url.fragment
                , fragment = Nothing
            }
    in
    urlWithoutFragment
        |> Url.parse urlParser
        |> Maybe.withDefault NotFoundPage
