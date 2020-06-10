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
                column [ width fill ] requestView

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

&nbsp;
&nbsp;

The "Request" menu allows you to create and run HTTP request.

&nbsp;

## Use environment variable

&nbsp;
&nbsp;

Variables are available through the mustache syntax.

If you have variable `userId` in your environment you use can use it in any text input with `{{userId}}`

&nbsp;
&nbsp;

## Limitations

&nbsp;
&nbsp;

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


-- ** md renderer


elmUiRenderer : Md.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = paragraph [  ]
    , thematicBreak = Element.none
    , text = \t -> text t
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            link
                [ htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph [ Font.color (Element.rgb255 0 0 255) ] body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , padding 10
                , Border.color (Element.rgb255 145 145 145)
                , Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Md.ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        Md.IncompleteTask ->
                                            Input.defaultCheckbox False

                                        Md.CompletedTask ->
                                            Input.defaultCheckbox True

                                        Md.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = MdH.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    , tableCell = Element.paragraph []
    }


heading : { level : Md.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Md.H1 ->
                    36

                Md.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Region.heading (Md.headingLevelToInt level)
        , htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children

code : String -> Element msg
code snippet =
    Element.el
        [ Background.color
            (Element.rgba 0 0 0 0.04)
        , Border.rounded 2
        , paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)

codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Background.color (Element.rgba 0 0 0 0.03)
        , htmlAttribute (Html.Attributes.style "white-space" "pre")
        , padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)

rawTextToId rawText =
    rawText
        |> String.split " "
        |> Debug.log "split"
        |> String.join "-"
        |> Debug.log "joined"
        |> String.toLower


-- * markdown


renderMarkdown : String -> List (Element msg)
renderMarkdown rawMarkdown =
    let
        markdown =
            rawMarkdown
                 |> Md.parse
                 |> Result.mapError (\error -> error |> List.map Md.deadEndToString |> String.join "\n")
                 |> Result.andThen (Md.render elmUiRenderer)
    in
    case markdown of
        Ok html ->
            html

        Err errs ->
            [ text errs ]
