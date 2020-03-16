module Page exposing (Page(..), urlToPage, href)

import Url
import Url.Parser.Query as Query
import Url.Parser as Url exposing ((</>), (<?>))
import Uuid


-- * model


type Page
    = HomePage
    | ReqPage (Maybe Uuid.Uuid)
    | EnvPage
    | OAuthCallbackPage String
    | SignInPage
    | SignUpPage
    | InitializePasswordPage Uuid.Uuid String
    | SignOutPage
    | NotFoundPage


-- * parser


uuidParser =
    Url.custom "UUID" Uuid.fromString


urlParser : Url.Parser (Page -> a) a
urlParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map (\id -> ReqPage (Just id)) (Url.s "req" </> uuidParser)
        , Url.map (ReqPage Nothing) (Url.s "req")
        , Url.map EnvPage (Url.s "env")
        , Url.map SignInPage (Url.s "signIn")
        , Url.map SignUpPage (Url.s "signUp")
        , Url.map InitializePasswordPage (Url.s "account" </> uuidParser </> Url.s "initializePassword" </> Url.string)
        , Url.map SignOutPage (Url.s "signOut")
        ]


-- * href


href : Page -> String
href page =
    let
        pieces =
            case page of
                HomePage ->
                    []

                ReqPage (Just uuid) ->
                    ["req", Uuid.toString uuid]

                ReqPage Nothing ->
                    ["req"]

                EnvPage ->
                    ["env"]

                SignInPage ->
                    ["signIn"]

                SignUpPage ->
                    ["signUp"]

                InitializePasswordPage accountId token ->
                    [ "account"
                    , Uuid.toString accountId
                    , "initializePassword"
                    , token
                    ]

                SignOutPage ->
                    [ "settings" ]

                OAuthCallbackPage _ ->
                    [ "oauth"
                    , "callback"
                    ]

                NotFoundPage ->
                    [ "notFound" ]
    in
        "#" ++ String.join "/" pieces


-- * util


urlToPage : Url.Url -> Page
urlToPage url =
    let
        {-
        when dealing with github oauth, the callback url cannot contains '#'
        so we instead returns the root url with only a 'code' query param
        eg: host.com/?code=someCode
        -}
        parseOAuth : Maybe Page
        parseOAuth =
            case Url.parse (Url.query (Query.string "code")) url of
                Just (Just code) ->
                    Just (OAuthCallbackPage code)

                _ ->
                    Nothing

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
        case parseOAuth of
            Just oauthPage ->
                oauthPage

            Nothing ->
                urlWithoutFragment
                    |> Url.parse urlParser
                    |> Maybe.withDefault NotFoundPage
