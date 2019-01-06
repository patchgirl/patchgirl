import Browser
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import Debug
import Url
import Json.Decode as Json
import HttpResponse
import Message exposing (Msg(..))
import HttpRequestValidity
import HttpMethod exposing (Model(..))
import HttpHeader
import HttpUrl

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : (HttpUrl.Model, HttpRequestValidity.Model, Maybe HttpResponse.Model) -> Sub Msg
subscriptions _ =
  Sub.none

init : () -> ((HttpUrl.Model, HttpRequestValidity.Model, Maybe HttpResponse.Model), Cmd Msg)
init _ =
  let
    model = { url = "swapi.co/api/people/1", httpScheme = "HTTP", httpMethod = Get, httpHeaders = [] }
    httpRequestValidity = { urlValid = False, httpHeadersValid = True }
    httpResponse = Nothing
  in ((model, httpRequestValidity, httpResponse), Cmd.none)

-- UPDATE

update : Msg
       -> (HttpUrl.Model, HttpRequestValidity.Model, Maybe HttpResponse.Model)
       -> ((HttpUrl.Model, HttpRequestValidity.Model, Maybe HttpResponse.Model), Cmd Msg)
update msg (model, validity, mHttpResponse) =
  case msg of
    UpdateUrl url ->
      case HttpUrl.parseUrl model url of
        Just u -> ( ( { model | url = url }
                    , { validity | urlValid = True }
                    , mHttpResponse)
                  , Cmd.none)
        Nothing -> ( ( { model | url = url }
                     , { validity | urlValid = False }
                     , mHttpResponse)
                   , Cmd.none)

    SetHttpMethod newMethod ->
      case newMethod of
        "GET" -> ( ({ model | httpMethod = Get }
                   , validity
                   , mHttpResponse)
                 , Cmd.none)
        _ -> ( ({ model | httpMethod = Post }
               , validity
               , mHttpResponse)
             , Cmd.none)

    SetHttpScheme scheme ->
      case scheme of
        "HTTP" -> ( ({ model | httpScheme = "HTTP" }
                    , validity
                    , mHttpResponse)
                  , Cmd.none)
        _ -> ( ({ model | httpScheme = "HTTPS" }
               , validity
               , mHttpResponse)
             , Cmd.none)

    GetHttpResponse result ->
      ((model, validity, Just result), Cmd.none)

    UpdateHeaders rawHeaders ->
      case HttpHeader.parseHeaders rawHeaders of
        Just httpHeaders ->
          ( ( { model | httpHeaders = httpHeaders }
            , { validity | httpHeadersValid = True }
            , mHttpResponse)
          , Cmd.none)
        Nothing ->
          ( (model
            , { validity | httpHeadersValid = False }
            , mHttpResponse)
          , Cmd.none )

    RunHttpRequest ->
      let
        httpRequest = Http.request
          { method = HttpMethod.toString model.httpMethod
          , headers = List.map HttpHeader.mkHeader model.httpHeaders
          , url = HttpUrl.fullUrl model
          , body = Http.emptyBody
          , expect = Http.expectString GetHttpResponse
          , timeout = Nothing
          , tracker = Nothing
          }
      in ((model, validity, mHttpResponse), httpRequest)

-- VIEW

view : (HttpUrl.Model, HttpRequestValidity.Model, Maybe HttpResponse.Model) -> Html Msg
view (model, httpRequestValidity, mHttpResponse) =
  div []
    [ HttpUrl.view (model, httpRequestValidity)
    , HttpHeader.view httpRequestValidity
    , HttpResponse.view mHttpResponse
    ]
