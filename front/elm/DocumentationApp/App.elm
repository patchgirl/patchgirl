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
                column [] requestView

            ScenarioDoc ->
                none

            EnvironmentDoc ->
                none


-- ** request


requestView : List (Element msg)
requestView =
    let
        markdown =
            """
# Request

The "Request" menu allows you to create and run HTTP request.

## Use environment variable

Variables are available through the mustache syntax.

If you have variable `userId` in your environment you use can use it in any text input with `{{userId}}`

## Limitations

Due to browser restrictions (cf: [CORS](https://developer.mozilla.org/docs/Web/HTTP/CORS) and [same origin policy](https://developer.mozilla.org/docs/Web/Security/Same_origin_policy_for_JavaScript)), PatchGirl cannot run from your device.
Instead, every requests you make are being sent from our servers which means that you can't by default query local hosts (eg: 127.0.0.1) or use your personnal VPN/Proxy.
To overcome this issue, you need to download the **PatchGirl Desktop App** which runs on your computer and allows you to run requests from your device.
    """
    in
    renderMarkdown markdown


-- ** scenario


scenarioView : Element msg
scenarioView =
    text "Scen Le Lorem Ipsum est simplement du faux texte employé dans la composition et la mise en page avant impression. Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années 1500, quand un imprimeur anonyme assembla ensemble des morceaux de texte pour réaliser un livre spécimen de polices de texte. Il n'a pas fait que survivre cinq siècles, mais s'est aussi adapté à la bureautique informatique, sans que son contenu n'en soit modifié. Il a été popularisé dans les années 1960 grâce à la vente de feuilles Letraset contenant des passages du Lorem Ipsum, et, plus récemment, par son inclusion dans des applications de mise en page de texte, comme Aldus PageMaker."


-- ** environment


environmentView : Element msg
environmentView =
    text "Le Lorem Ipsum est simplement du faux texte employé dans la composition et la mise en page avant impression. Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années 1500, quand un imprimeur anonyme assembla ensemble des morceaux de texte pour réaliser un livre spécimen de polices de texte. Il n'a pas fait que survivre cinq siècles, mais s'est aussi adapté à la bureautique informatique, sans que son contenu n'en soit modifié. Il a été popularisé dans les années 1960 grâce à la vente de feuilles Letraset contenant des passages du Lorem Ipsum, et, plus récemment, par son inclusion dans des applications de mise en page de texte, comme Aldus PageMaker."


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
