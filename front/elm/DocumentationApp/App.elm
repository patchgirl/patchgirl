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
import Util exposing (..)


-- * view


view : Documentation -> Element msg
view documentation =
    wrappedRow [ width fill, spacing 10, paddingXY 10 0 ]
        [ navView documentation
        , contentView documentation
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


-- ** patchgirl runner app


patchgirlRunnerAppView : String
patchgirlRunnerAppView =
    """
# PatchGirl Runner App

The **PatchGirl runner app** (that you can [download](https://github.com/patchgirl/patchgirl/releases/) here) allows http requests and scenarios to be run from your computer instead of from our servers.
This is quite useful if you need to:
- query a local host (i.e: *127.0.0.1*)
- query a host through a VPN/Proxy you've defined on your computer

## Available platform

As of today, the PatchGirl runner app is only available on Linux. We plan to make it available on MacOS and Windows as well soon enough.

## How to use

Installation is quite easy because the PatchGirl runner app is a standalone executable. After downloading it, you only need to run it:

**`chmod +x ./linux-x86-64-patchgirl-runner-exe`**

**`./linux-x86-64-patchgirl-runner-exe`**

Once it is running you should see 2 green arrows appearing in the [UI](https://patchgirl.io/#app/scenario) in the top right hand corner.
This means that PatchGirl Runner is enabled and that you can play all kinds of requests (http, postgres, scenarios...).
    """


-- ** request


requestView : String
requestView =
    """
# Request

The "Request" menu allows you to create and run HTTP request.

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` set in your environment you can use it in any text input with `{{userId}}`

## Limitations

Due to browser restrictions (cf: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS) and [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), PatchGirl cannot run from your computer.
Instead, every requests you make are being sent from our servers which means that you can't by default query local hosts (i.e: `127.0.0.1`) or use your personnal VPN/Proxy.
To overcome this issue, you need to download and run the [**PatchGirl Desktop App**](https://github.com/patchgirl/patchgirl/releases) (only available for Linux at the time) which runs on your computer and allows you to run requests from your device.
The PatchGirl desktop app acts as a tiny proxy. You still use the web app as an interface but every calls are being sent from the desktop app instead of our servers.
    """


-- ** postgresql


postgresqlView : String
postgresqlView =
    """
# Postgres

The "Postgres" menu allows you to create and run Postgres SQL queries .

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` set in your environment you can use it in any text input with `{{userId}}`.
For example, you might want to put your postgres connection info into some environment variables to save you from constantly re typing your credentials.

## Warning

Any saved postgresql request will be saved in clear on our servers. This means that your credentials won't be encrypted!

PatchGirl is a tool that aims to help testing your local/staging environment and you should **NEVER** store any critical information in it!

## Limitations

Due to browser restrictions (cf: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS) and [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), PatchGirl cannot run from your computer.
Instead, every requests you make are being sent from our servers which means that you can't by default query local hosts (i.e: `127.0.0.1`) or use your personnal VPN/Proxy.
To overcome this issue, you need to download and run the [**PatchGirl Desktop App**](https://github.com/patchgirl/patchgirl/releases) (only available for Linux at the time) which runs on your computer and allows you to run requests from your device.
The PatchGirl desktop app acts as a tiny proxy. You still use the web app as an interface but every calls are being sent from the desktop app instead of our servers.
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

The environment menu allows you to create variables that you can reuse in your http request.

## use variable

Once defined, a variable can easily be used in an http request text inputs or in a scenario's script input.
The syntax used is the mustache syntax i.e: `http://{{host}}/somePath`

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
