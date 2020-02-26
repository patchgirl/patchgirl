module Page exposing (Page(..), urlToPage, href)

import Url as Url
import Url.Parser as Url exposing ((</>))
import Uuid

type Page
    = HomePage
    | ReqPage (Maybe Uuid.Uuid)
    | EnvPage
    | SignInPage
    | SignUpPage
    | InitializePasswordPage Int String
    | SignOutPage

urlToPage : Url.Url -> Page
urlToPage url =
    {-
      The RealWorld spec treats the fragment like a path.
      This makes it *literally* the path, so we can proceed
      with parsing as if it had been a normal path all along.
    -}
    let urlWithoutFragment =
            { url
                | path = Maybe.withDefault "" url.fragment
                , fragment = Nothing
            }
    in
        urlWithoutFragment
            |> Url.parse urlParser
            |> Maybe.withDefault HomePage



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
        , Url.map InitializePasswordPage (Url.s "account" </> Url.int </> Url.s "initializePassword" </> Url.string)
        , Url.map SignOutPage (Url.s "signOut")
        ]

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
                    ["account", String.fromInt accountId, "initializePassword", token]

                SignOutPage ->
                    [ "settings" ]
    in
        "#/" ++ String.join "/" pieces
