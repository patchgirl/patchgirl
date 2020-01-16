module Page exposing (Page(..), urlToPage)

import Url as Url
import Url.Parser as Url exposing ((</>))

type Page
    = HomePage
    | ReqPage
    | EnvPage
    | SignInPage
    | SignUpPage


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


urlParser : Url.Parser (Page -> a) a
urlParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map ReqPage (Url.s "req")
        , Url.map EnvPage (Url.s "env")
        , Url.map SignInPage (Url.s "signIn")
        , Url.map SignUpPage (Url.s "signUp")
        ]
