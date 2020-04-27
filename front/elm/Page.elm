module Page exposing (Page(..), href, urlToPage)

import Url
import Url.Parser as Url exposing ((</>))
import Uuid



-- * model


type Page
    = HomePage
    | ReqPage (Maybe Uuid.Uuid) (Maybe Uuid.Uuid)
    | EnvPage
    | ScenarioPage (Maybe Uuid.Uuid)
    | NotFoundPage



-- * parser


uuidParser : Url.Parser (Uuid.Uuid -> b) b
uuidParser =
    Url.custom "UUID" Uuid.fromString


urlParser : Url.Parser (Page -> a) a
urlParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map (\reqId scenarioId -> ReqPage (Just reqId) (Just scenarioId)) (Url.s "req" </> uuidParser </> uuidParser)
        , Url.map (\id -> ReqPage (Just id) Nothing) (Url.s "req" </> uuidParser)
        , Url.map (ReqPage Nothing Nothing) (Url.s "req")
        , Url.map EnvPage (Url.s "env")
        , Url.map (\id -> ScenarioPage (Just id)) (Url.s "scenario" </> uuidParser)
        , Url.map (ScenarioPage Nothing) (Url.s "scenario")
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
                    [ "req", Uuid.toString reqId, Uuid.toString scenarioId ]

                ReqPage (Just uuid) Nothing ->
                    [ "req", Uuid.toString uuid ]

                ReqPage Nothing _ ->
                    [ "req" ]

                EnvPage ->
                    [ "env" ]

                ScenarioPage (Just uuid) ->
                    [ "scenario", Uuid.toString uuid ]

                ScenarioPage Nothing ->
                    [ "scenario" ]

                NotFoundPage ->
                    [ "notFound" ]
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
