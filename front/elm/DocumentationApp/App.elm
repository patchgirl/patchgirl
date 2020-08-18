module DocumentationApp.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Page exposing (..)
import Parser.Advanced as PA
import Util
import List.Extra as List
import Parser as P
import Markdown.Parser as Md
import Markdown.Html as MdH
import Markdown.Renderer as Md
import Markdown.Block as Md
import Html
import Html.Attributes as Html
import Util exposing (..)
import Browser.Dom as Dom
import Task


-- * model


type alias DocPage =
    { content : List Md.Block
    , menu : Maybe DocMenu
    }

type alias DocMenu =
    (String, List (Md.HeadingLevel, String))


-- * message


type Msg
    = Focus String
    | NoOp


-- * update


update : Msg -> Cmd Msg
update msg =
    case msg of
        NoOp ->
            Cmd.none

        Focus title ->
            Dom.getElement title
                |> Task.andThen (\info -> Dom.setViewport 0 (info.element.y - 10))
                |> Task.attempt (always NoOp)



-- * view


view : Documentation -> Element Msg
view currentDocumentation =
    let
        contents : Result String (List DocPage)
        contents = List.map parse [ requestView
                                  , postgresqlView
                                  , scenarioView
                                  , environmentView
                                  , patchgirlRunnerAppView
                                  ] |> traverseListResult

        mkMenuItem : DocMenu -> Element Msg
        mkMenuItem (h1, children) =
            let
                isSelectedMenu =
                    documentationToString currentDocumentation == normalizeTitle h1

                selectedMenuAttrs =
                    case isSelectedMenu of
                        True -> Font.bold
                        False -> Font.regular
            in
            column [ spacing 5 ]
                [ link [ selectedMenuAttrs ]
                      { url =
                            normalizeTitle h1
                              |> documentationFromString
                              |> Maybe.withDefault RequestDoc
                              |> DocumentationPage
                              |> href
                      , label = text h1
                      }
                , column [ spacing 5 ] <|
                    case isSelectedMenu of
                        True -> List.map (mkSubMenuItem h1) children
                        False -> []
                ]

        mkSubMenuItem : String -> (Md.HeadingLevel, String) -> Element Msg
        mkSubMenuItem h1 (level, title) =
            case level of
                Md.H4 -> none
                Md.H5 -> none
                Md.H6 -> none
                _ ->
                    Input.button [ Font.size (headingLevelFontSize level)
                                 , paddingXY (headingLevelPadding level) 0
                                 ]
                        { onPress = Just (Focus (normalizeTitle title))
                        , label = text title
                        }


        mkMenu : List DocPage -> List (Element Msg)
        mkMenu docPages =
            List.map .menu docPages
                |> catMaybes
                |> (\menus -> List.map mkMenuItem menus)
    in
        wrappedRow [ width fill, spacing 10, paddingXY 10 0]
            [ case contents of
                Err str -> text ("can't generate documentation: " ++ str)
                Ok docPages ->
                    column [ Background.color white
                           , boxShadow
                           , spacing 20
                           , padding 20
                           , alignTop
                           ] (mkMenu docPages)
            , contentView currentDocumentation
            ]


-- * navigation


navView : Documentation -> Element msg
navView documentation =
    column [ Background.color white
           , boxShadow
           , spacing 20
           , padding 20
           , alignTop
           ]
        [ documentationLink documentation RequestDoc "Request"
        , documentationLink documentation PostgresDoc "Postgresql"
        , documentationLink documentation ScenarioDoc "Scenario"
        , documentationLink documentation EnvironmentDoc "Environment"
        , documentationLink documentation PatchGirlRunnerAppDoc "PatchGirl Runner App"
        ]

documentationLink : Documentation -> Documentation -> String -> Element msg
documentationLink currentDocumentation documentation title =
    let
        selected : Bool
        selected =
            currentDocumentation == documentation
    in
    link []
        { url = href (DocumentationPage (documentation))
        , label =
            case selected of
                True ->
                    el [ paddingXY 20 0 ] <|
                        iconWithAttr { defaultIconAttribute
                                         | title = title
                                         , icon = "keyboard_arrow_right"
                                     }
                False -> el [] (text title)
        }


-- * content


contentView : Documentation -> Element msg
contentView documentation =
    let
        doc : String -> Element msg
        doc markdown =
            column [ boxShadow
                   , Background.color white
                   , padding 20
                   ] (renderMarkdown markdown)
    in
    el [ width (fill |> maximum 800), centerX ] <|
        case documentation of
            RequestDoc ->
                doc requestView

            PostgresDoc ->
                doc postgresqlView

            ScenarioDoc ->
                doc scenarioView

            EnvironmentDoc ->
                doc environmentView

            PatchGirlRunnerAppDoc ->
                doc patchgirlRunnerAppView


-- ** request


requestView : String
requestView =
    """
# HTTP Request

The "HTTP" menu allows you to create and run HTTP request.

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` set in your environment or in a script, you can use it in any text input with the syntax `{{userId}}`

## Limitations

Due to browser restrictions (cf: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS) and [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), PatchGirl cannot run from your computer.
Instead, every requests you make are being sent from our servers which means that you can't by default query local hosts (i.e: `127.0.0.1`) or use your personnal VPN/Proxy.

To overcome this issue, you need to download and run [**PatchGirl Runner**](https://github.com/patchgirl/patchgirl/releases) (only available for Linux and MacOS at the time) which runs on your computer and allows you to run HTTP requests from your device.

PatchGirl-runner acts as a tiny proxy. You still use the web app as an interface but every calls are being sent from the runner app instead of our servers.
    """


-- ** postgresql


postgresqlView : String
postgresqlView =
    """
# Postgres

The "Postgres" menu allows you to create and run Postgres SQL queries .

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` set in your environment or in a script, you can use it in any text input with the syntax `{{userId}}`.
For example, you might want to put your postgres connection info into some environment variables to save you from constantly re typing your credentials.

## Warning

Any saved postgresql request will be saved in clear on our servers. This means that your credentials won't be encrypted!

PatchGirl is a tool that aims to help testing your local/staging environment and you should **NEVER** store any critical information in it!

## Limitations

Due to browser limitations, PatchGirl cannot run SQL.

To overcome this issue, you need to download and run [**PatchGirl-Runner**](https://github.com/patchgirl/patchgirl/releases) (only available for Linux and MacOS at the time) which runs on your computer and allows you to run SQL requests from your device.

PatchGirl-runner acts as a tiny proxy. You still use the web app as an interface but every calls are being sent from the runner app instead of our servers.
    """


-- ** scenario


scenarioView : String
scenarioView =
    """
# Scenario

The "Scenario" menu allows you to play HTTP and postgres SQL requests sequentially.

## Requests

Only existing http or postgres requests can be added to a scenario.

## [Pre|Post] Script

You can add script before and after every http requests of a scenario. This might come in handy if you need to override a variable before executing a request or if you need to test the response of a request.
Script uses a syntax similar to javascript.

### Local variable

In your [pre|post] script, you can define local variable with the syntax:

**`var foo = 1;`**

Local variable's scope is only limited to the script they are being defined in.

### Global variable

You can define a variable that will live through out its script with the syntax:

**`set("foo", 1);`**

You can use a global variable with this syntax:

**`get("foo");`**

Note that global variable's scope is only limited to the scenario they are defined in.
For variable that lives everywhere, you need to use environment variables.

### Assertion

When you run a scenario, you can make assertions in a script with the `assertEqual` function. Its syntax is:

**`assertEqual("someValue", "someOtherValue");`**

If both parameter are of the same type and same value, then the script succeeds.
When an assertion fails, every following scenes of a scenario won't be ran.


#### Http assertion

In a post script, some utilities are available:
- **`httpResponseBodyAsString`** returns the response body as a string
- **`httpResponseStatus`** returns the response status as an int

So if you want to check that the response succeeded you can write: **`assertEqual(httpResponseStatus, 200);`**

"""


-- ** environment


environmentView : String
environmentView =
    """
# Environment variables

The environment menu allows you to create variables that you can reuse in almost everywhere.

## How to use environment variable

Once defined, a variable can easily be used in any:
    - http request text inputs
    - postgresql query text inputs


The syntax used is the mustache syntax e.g: `http://{{host}}/somePath`
     """


-- ** patchgirl runner


patchgirlRunnerAppView : String
patchgirlRunnerAppView =
    """
# PatchGirl Runner

The **PatchGirl runner app** (that you can [download](https://github.com/patchgirl/patchgirl/releases/) here) allows http requests and scenarios to be run from your computer instead of from our servers.
This is quite useful if you need to:
- query a local host (i.e: *127.0.0.1*)
- query a host through a VPN/Proxy you've defined on your computer

## Available platforms

As of today, the PatchGirl runner app is only available on Linux and MacOS. We plan to make it available on Windows as well soon enough.

## How to use

Installation is quite easy because the PatchGirl runner app is a standalone executable. After downloading it, you only need to run it:

**`chmod +x ./linux-x86-64-patchgirl-runner-exe`**

**`./linux-x86-64-patchgirl-runner-exe`**

Once it is running you should see 2 green arrows appearing in the [UI](https://patchgirl.io/#app/scenario) in the top right hand corner.
This means that PatchGirl Runner is enabled and that you can play all kinds of requests (HTTP, SQL, scenarios...) straight from your computer.
    """


-- * markdown


renderMarkdown : String -> List (Element msg)
renderMarkdown rawMarkdown =
    let
        renderer =
            let defaultRenderer = Md.defaultHtmlRenderer
            in { defaultRenderer | heading = htmlHeadingRenderer }

        markdown =
            rawMarkdown
                 |> Md.parse
                 |> Result.mapError (\error -> error |> List.map Md.deadEndToString |> String.join "\n")
                 |> Result.andThen (Md.render renderer)

    in
    case markdown of
        Ok html ->
            html
                |> List.map Element.html
                |> \element -> [ paragraph [] element ]

        Err errs ->
            [ text errs ]


-- ** parse


parse : String -> Result String DocPage
parse rawMarkdown =
    let
        markdown : Result String (List Md.Block)
        markdown =
            rawMarkdown
                 |> Md.parse
                 |> Result.mapError (\error -> error |> List.map Md.deadEndToString |> String.join "\n")

        mkMenu : List (Md.HeadingLevel, String) -> Maybe DocMenu
        mkMenu headers =
            let
                f : (Md.HeadingLevel, String) -> Maybe DocMenu -> Maybe DocMenu
                f (level, title) mAcc =
                    case level of
                        Md.H1 -> Just (title, [])
                        _ ->
                            mAcc |> Maybe.map (\(h1, h1Children) -> (h1, h1Children ++ [ (level, title) ]))
            in
            List.foldl f Nothing headers

        toDocPage : List Md.Block -> { headers : List (Md.HeadingLevel, String)
                                     , content : List Md.Block
                                     }
        toDocPage blocks =
            let
                convertBlockToHeader : Md.Block -> Maybe (Md.HeadingLevel, String)
                convertBlockToHeader block =
                    case block of
                        Md.Heading level inlines ->
                            case inlines of
                                (Md.Text title :: _) ->
                                    Just (level, title)
                                _ -> Nothing

                        _ -> Nothing
            in
            List.map convertBlockToHeader blocks
                |> catMaybes
                |> \headers -> { headers = headers, content = blocks }

    in
    markdown
        |> Result.map toDocPage
        |> Result.map (\{ headers, content } -> { content = content
                                                , menu = mkMenu headers
                                                }
                      )



-- ** header renderer


htmlHeadingRenderer : { level : Md.HeadingLevel
                      , rawText : String
                      , children : List (Html.Html view)
                      }
                    -> Html.Html view
htmlHeadingRenderer { level, rawText, children } =
    let
        htmlId = normalizeTitle rawText |> Html.id
    in
    case level of
        Md.H1 ->
            Html.h1 [ htmlId ] children

        Md.H2 ->
            Html.h2 [ htmlId ] children

        Md.H3 ->
            Html.h3 [ htmlId ] children

        Md.H4 ->
            Html.h4 [ htmlId ] children

        Md.H5 ->
            Html.h5 [ htmlId ] children

        Md.H6 ->
            Html.h6 [ htmlId ] children

normalizeTitle : String -> String
normalizeTitle title =
    String.words title
        |> String.join "-"
        |> String.toLower


-- * util


headingLevelFontSize level =
    case level of
        Md.H1 -> 20
        Md.H2 -> 18
        Md.H3 -> 16
        Md.H4 -> 14
        Md.H5 -> 12
        Md.H6 -> 10

headingLevelPadding level =
    case level of
        Md.H1 -> 0
        Md.H2 -> 10
        Md.H3 -> 20
        Md.H4 -> 30
        Md.H5 -> 40
        Md.H6 -> 50
