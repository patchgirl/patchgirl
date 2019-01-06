import Browser
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick)
import Http exposing (get, post)
import Debug
import Maybe.Extra
import Url
import Regex

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

subscriptions : (Model, HttpRequestValidity) -> Sub Msg
subscriptions _ =
  Sub.none

type HttpMethod = Get | Post

httpMethodToString : HttpMethod -> String
httpMethodToString httpMethod =
  case httpMethod of
    Get -> "GET"
    Post -> "POST"

type alias Model =
  { url : String
  , httpScheme : String
  , httpMethod : HttpMethod
  , httpHeaders : List (String, String)
  }

type alias HttpRequestValidity =
  { urlValid : Bool,
    httpHeadersValid : Bool
  }

init : () -> ((Model, HttpRequestValidity), Cmd Msg)
init _ =
  let
    model = { url = "", httpScheme = "HTTP", httpMethod = Get, httpHeaders = [] }
    httpRequestValidity = { urlValid = False, httpHeadersValid = True }
  in ((model, httpRequestValidity), Cmd.none)

fullUrl : Model -> String
fullUrl model =
  model.httpScheme ++ "://" ++ model.url

parseUrl : Model -> String -> Maybe Url.Url
parseUrl model url =
  let
    scheme = case model.httpScheme of
      "HTTP" -> "http"
      _ -> "https"
    urlRegex = Maybe.withDefault Regex.never <| Regex.fromString "(\\w+\\.\\w{2,}.*)|(localhost.*)"
    validUrl = Regex.contains urlRegex url
  in
    case validUrl of
      True -> Url.fromString <| scheme ++ "://" ++ url
      False -> Nothing

parseHeaders : String -> Maybe (List(String, String))
parseHeaders headers =
  let
    parseRawHeader : String -> Maybe(String, String)
    parseRawHeader rawHeader =
      case String.split ":" rawHeader of
        [headerKey, headerValue] -> Just (headerKey, headerValue)
        _ -> Nothing
  in
    Maybe.Extra.traverse parseRawHeader (
      String.lines headers |> List.filter (not << String.isEmpty)
    )

-- UPDATE

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | RunHttpRequest
  | GetHttpResponse (Result Http.Error String)
  | UpdateHeaders String
  | SetHttpScheme String

update : Msg -> (Model, HttpRequestValidity) -> ((Model, HttpRequestValidity), Cmd Msg)
update msg (model, validity) =
  case msg of
    UpdateUrl url ->
      case  parseUrl model url of
        Just u -> ( ( { model | url = url }
                    , { validity | urlValid = True })
                  , Cmd.none)
        Nothing -> ( ( { model | url = url }
                     , { validity | urlValid = False })
                   , Cmd.none)

    SetHttpMethod newMethod ->
      case newMethod of
        "GET" -> (({ model | httpMethod = Get }, validity), Cmd.none)
        _ -> (({ model | httpMethod = Post }, validity), Cmd.none)

    SetHttpScheme scheme ->
      case scheme of
        "HTTP" -> (({ model | httpScheme = "HTTP" }, validity), Cmd.none)
        _ -> (({ model | httpScheme = "HTTPS" }, validity), Cmd.none)

    GetHttpResponse result ->
      ((model, validity), Cmd.none)

    UpdateHeaders rawHeaders ->
      case parseHeaders rawHeaders of
        Just httpHeaders ->
          ( ( { model | httpHeaders = httpHeaders }
            , { validity | httpHeadersValid = True })
          , Cmd.none)
        Nothing ->
          ( (model, { validity | httpHeadersValid = False }), Cmd.none )

    RunHttpRequest ->
      let
        mkHeader : (String, String) -> Http.Header
        mkHeader (keyHeader, valueHeader) = Http.header keyHeader valueHeader
        httpRequest = Http.request
          { method = httpMethodToString model.httpMethod
          , headers = List.map mkHeader model.httpHeaders
          , url = fullUrl model
          , body = Http.emptyBody
          , expect = Http.expectString GetHttpResponse
          , timeout = Nothing
          , tracker = Nothing
          }
      in ((model, validity), httpRequest)

-- VIEW
httpMethodToOption : HttpMethod -> Html Msg
httpMethodToOption httpMethod = option [ Html.Attributes.value (httpMethodToString httpMethod) ] [ text <| httpMethodToString httpMethod ]

view : (Model, HttpRequestValidity) -> Html Msg
view (model, httpRequestValidity) =
  div []
    [ urlView (model, httpRequestValidity)
    , headersView (model, httpRequestValidity)
    ]

urlView : (Model, HttpRequestValidity) -> Html Msg
urlView (model, httpRequestValidity) =
  let
    status = case httpRequestValidity.urlValid of
      False -> "Invalid Url"
      True -> "Send"
  in
    div []
      [ select [ onInput SetHttpMethod ] ([Get, Post] |> List.map httpMethodToOption)
      , select [ onInput SetHttpScheme ]
        [ option [ Html.Attributes.value "HTTP" ] [ text "HTTP" ]
        , option [ Html.Attributes.value "HTTPS" ] [ text "HTTPS" ]
        ]
      , input [ placeholder "myApi.com/path?arg=someArg", value model.url, onInput UpdateUrl ] []
      , button [onClick RunHttpRequest, disabled <| not httpRequestValidity.urlValid] [ text status ]
      ]

headersView : (Model, HttpRequestValidity) -> Html Msg
headersView (model, httpRequestValidity) =
  let
    status = case httpRequestValidity.httpHeadersValid of
      False -> "Invalid Headers"
      True -> "Valid Headers"
  in
    div []
      [ textarea [ placeholder "Header: SomeHeader\nHeader2: SomeHeader2", onInput UpdateHeaders ] []
      , div [] [ text status ]
      ]
