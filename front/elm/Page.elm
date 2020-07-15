module Page exposing (Page(..), href, urlToPage, Documentation(..))

import Url
import Url.Parser as Url exposing ((</>))
import Uuid


-- * model


type Page
    = HomePage
    | ReqPage (Maybe Uuid.Uuid) (Maybe Uuid.Uuid)
    | PgPage (Maybe Uuid.Uuid)
    | EnvPage
    | ScenarioPage (Maybe Uuid.Uuid)
    | NotFoundPage
    | DocumentationPage Documentation
    | TangoScriptPage

type Documentation
    = ScenarioDoc
    | RequestDoc
    | PostgresDoc
    | EnvironmentDoc
    | PatchGirlRunnerAppDoc

documentationToString : Documentation -> String
documentationToString documentation =
    case documentation of
        ScenarioDoc ->
            "scenario"

        RequestDoc ->
            "request"

        PostgresDoc ->
            "postgres"

        EnvironmentDoc ->
            "environment"

        PatchGirlRunnerAppDoc ->
            "patchGirlRunnerApp"


-- * parser


uuidParser : Url.Parser (Uuid.Uuid -> b) b
uuidParser =
    Url.custom "UUID" Uuid.fromString

documentationParser : Url.Parser (Documentation -> b) b
documentationParser =
    let
        parser : String -> Maybe Documentation
        parser s =
            case s of
                "request" ->
                    Just RequestDoc

                "postgres" ->
                    Just PostgresDoc

                "scenario" ->
                    Just ScenarioDoc

                "environment" ->
                    Just EnvironmentDoc

                "patchGirlRunnerApp" ->
                    Just PatchGirlRunnerAppDoc

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
        , Url.map (\reqId scenarioId -> ReqPage (Just reqId) (Just scenarioId)) (appRoot </> Url.s "req" </> uuidParser </> uuidParser)
        , Url.map (\id -> ReqPage (Just id) Nothing) (appRoot </> Url.s "req" </> uuidParser)
        , Url.map (ReqPage Nothing Nothing) (appRoot </> Url.s "req")
        , Url.map (\id -> PgPage (Just id)) (appRoot </> Url.s "pg" </> uuidParser)
        , Url.map (PgPage Nothing) (appRoot </> Url.s "pg")
        , Url.map EnvPage (appRoot </> Url.s "env")
        , Url.map (\id -> ScenarioPage (Just id)) (appRoot </> Url.s "scenario" </> uuidParser)
        , Url.map (ScenarioPage Nothing) (appRoot </> Url.s "scenario")
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

                ReqPage (Just reqId) (Just scenarioId) ->
                    [ "app", "req", Uuid.toString reqId, Uuid.toString scenarioId ]

                ReqPage (Just uuid) Nothing ->
                    [ "app", "req", Uuid.toString uuid ]

                ReqPage Nothing _ ->
                    [ "app", "req" ]

                PgPage (Just uuid) ->
                    [ "app", "pg", Uuid.toString uuid ]

                PgPage Nothing ->
                    [ "app", "pg" ]

                EnvPage ->
                    [ "app", "env" ]

                ScenarioPage (Just uuid) ->
                    [ "app", "scenario", Uuid.toString uuid ]

                ScenarioPage Nothing ->
                    [ "app", "scenario" ]

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
