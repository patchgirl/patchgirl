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
    | EnvPage (Maybe Int)
    | ScenarioPage (Maybe Uuid) (Maybe Uuid)
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
        , Url.map (\reqId -> ReqPage (RunView reqId)) (appRoot </> Url.s "req" </> Url.s "run" </> uuidParser)

        -- pg
        , Url.map (PgPage (LandingView DefaultView)) (appRoot </> Url.s "pg")
        , Url.map (PgPage (LandingView CreateDefaultFolderView)) (appRoot </> Url.s "pg" </> Url.s "new-folder")
        , Url.map (PgPage (LandingView CreateDefaultFileView)) (appRoot </> Url.s "pg" </> Url.s "new-file")
        , Url.map (\pgId -> PgPage (EditView (DefaultEditView pgId))) (appRoot </> Url.s "pg" </> uuidParser </> Url.s "edit")
        , Url.map (\pgId -> PgPage (EditView (DeleteView pgId))) (appRoot </> Url.s "pg" </> uuidParser </> Url.s "edit" </> Url.s "delete")
        , Url.map (\pgId -> PgPage (RunView pgId)) (appRoot </> Url.s "pg" </> Url.s "run" </> uuidParser)

        , Url.map (\id -> EnvPage (Just id)) (appRoot </> Url.s "env" </> Url.int)
        , Url.map (EnvPage Nothing) (appRoot </> Url.s "env")
        , Url.map (\id1 id2 -> ScenarioPage (Just id1) (Just id2) ) (appRoot </> Url.s "scenario" </> uuidParser </> uuidParser)
        , Url.map (\id -> ScenarioPage (Just id) Nothing) (appRoot </> Url.s "scenario" </> uuidParser)
        , Url.map (ScenarioPage Nothing Nothing) (appRoot </> Url.s "scenario")
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

                                RunView id ->
                                    [ "run", Uuid.toString id ]

                    in
                    [ "app", "pg" ] ++ mode

                EnvPage Nothing ->
                    [ "app", "env" ]

                EnvPage (Just id) ->
                    [ "app", "env", String.fromInt id ]

                ScenarioPage mUuid1 mUuid2  ->
                    let
                        uuidToStr mUuid =
                            mUuid
                                |> Maybe.map Uuid.toString
                                |> Maybe.withDefault ""
                    in
                    [ "app", "scenario", uuidToStr mUuid1, uuidToStr mUuid2 ]

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
