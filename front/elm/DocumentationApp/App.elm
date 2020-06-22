module DocumentationApp.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Page exposing (..)
import Markdown.Parser as Md
import Markdown.Html as MdH
import Markdown.Renderer as Md
import Markdown.Block as Md
import Html
import Html.Attributes


-- * view


view : Documentation -> Element msg
view documentation =
    wrappedRow [ width fill, spacing 20, paddingXY 20 0 ]
        [ navView documentation
        , contentView documentation
        ]


-- * navigation


navView : Documentation -> Element msg
navView documentation =
    column [ spacing 20 ]
        [ documentationLink documentation RequestDoc "Request"
        , documentationLink documentation ScenarioDoc "Scenario"
        , documentationLink documentation EnvironmentDoc "Environment"
        ]

documentationLink : Documentation -> Documentation -> String -> Element msg
documentationLink currentDocumentation documentation title =
    let
        selected : Bool
        selected =
            currentDocumentation == documentation

        selectedLink : List (Attribute msg)
        selectedLink =
            case selected of
                False ->
                    []

                True ->
                    [ Font.bold, Font.underline, paddingXY 10 0 ]
    in
    link []
        { url = href (DocumentationPage (documentation))
        , label = el selectedLink (text title)
        }


-- * content


contentView : Documentation -> Element msg
contentView documentation =
    el [ width (fill |> maximum 800), centerX ] <|
        case documentation of
            RequestDoc ->
                column [] (renderMarkdown requestView)

            ScenarioDoc ->
                none

            EnvironmentDoc ->
                none


-- ** request


requestView : String
requestView =
    """
# Request

The "Request" menu allows you to create and run HTTP request.

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` set in your environment you can use it in any text input with `{{userId}}`.

## Limitations

Due to browser restrictions (cf: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS) and [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), PatchGirl cannot run from your device.
Instead, every requests you make are being sent from our servers which means that you can't by default query local hosts (eg: 127.0.0.1) or use your personnal VPN/Proxy.
To overcome this issue, you need to download and run the **PatchGirl Desktop App** (only available for Linux at the time) which runs on your computer and allows you to run requests from your device.
The PatchGirl desktop app acts as a tiny proxy. You still use the web app but every calls are being sent from the desktop app instead of our servers.
    """


-- ** scenario


scenarioView : String
scenarioView =
    """
# Scenario

The "Scenario" menu allows you to sequentially play HTTP requests.

## Http Requests

Once your http requests are created, they can be added to your scenario.

## [Pre|Post] Script

You can add script before and after every http requests of a scenario. This might come in handy if you need to override a variable before executing a request or if you need to test the response of a request.

### Local variable

In your script, you can define local variable with the syntax `var foo = 1;`
Local variable's scope is only limited to the script they are being defined in.

### Global variable

You can define a variable that will live through out its script with the syntax: `set("foo", 1);`
You can use a global variable with this syntax: `get("foo");`
Note that global variable's scope is only limited to the scenario they are defined in.
For variable that lives everywhere, you need to use environment variables.

### Assertion

When you run a scenario, you can make assertions in a script with the `assertEqual` function. Its syntax is `assertEqual("someString", "someOtherString");`. If both parameter are of the same type and same value, then the script succeeds.
When an assertion fails, every following scenes won't be ran.

In a post script, some utilities are available:
- `httpResponseBodyAsString` returns the response body as a string
- `httpResponseStatus` returns the response status as an int

So if you want to check that the response succeeded you can write: `assertEqual(httpResponseStatus, 200);`


## Limitations

"""


-- ** environment


environmentView : String
environmentView =
    """
# Environment variables

The environment menu allows you to create variables that you can reuse in your http request.

## use variable

Once defined, a variable can easily be used in an http request text inputs or in a scenario's script.
The syntax used is the mustache syntax, e.g: `http://{{host}}/somePath`

     """


-- * markdown


renderMarkdown : String -> List (Element msg)
renderMarkdown rawMarkdown =
    let
        markdown =
            rawMarkdown
                 |> Md.parse
                 |> Result.mapError (\error -> error |> List.map Md.deadEndToString |> String.join "\n")
                 |> Result.andThen (Md.render (Md.defaultHtmlRenderer))

    in
    case markdown of
        Ok html ->
            html
                |> List.map Element.html
                |> \element -> [ paragraph [] element ]

        Err errs ->
            [ text errs ]
