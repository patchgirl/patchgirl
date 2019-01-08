module Builder.App exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled, class, id)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import Debug
import Url
import Json.Decode as Json

import Builder.Response
import Builder.Message exposing (Msg(..))
import Builder.Model exposing (Model, Method(..))
import Builder.Header
import Builder.Url
import Builder.Body
import Builder.Tree
import Builder.Method

defaultModel : Model
defaultModel =
  { name = "no name"
  , url = "swapi.co/api/people/1"
  , scheme = "HTTP"
  , method = Get
  , headers = []
  , body = ""
  , validity = { url = False, headers = True }
  , response = Nothing
  }

init : () -> (Model, Cmd Msg)
init _ = (defaultModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateUrl url ->
      case Builder.Url.parseUrl model url of
        Just u ->
          let
            oldValidity = model.validity
            newValidity = { oldValidity | url = True }
          in
            ( { model | url = url, validity = newValidity }, Cmd.none)
        Nothing ->
          let
            oldValidity = model.validity
            newValidity = { oldValidity | url = False }
          in ( { model | url = url, validity = newValidity }, Cmd.none)

    SetHttpMethod newMethod ->
      case newMethod of
        "GET" -> ( { model | method = Get }, Cmd.none)
        _ -> ( { model | method = Post }, Cmd.none)

    SetHttpScheme scheme ->
      case scheme of
        "HTTP" -> ( { model | scheme = "HTTP" }, Cmd.none)
        _ -> ( { model | scheme = "HTTPS" }, Cmd.none)

    GetHttpResponse result ->
      ( { model | response = Just result }, Cmd.none)

    UpdateHeaders rawHeaders ->
      case Builder.Header.parseHeaders rawHeaders of
        Just headers ->
          let
            oldValidity = model.validity
            newValidity = { oldValidity | headers = True }
          in
            ( { model | validity = newValidity, headers = headers }, Cmd.none )
        Nothing ->
          let
            oldValidity = model.validity
            newValidity = { oldValidity | headers = False }
          in
            ( { model | validity = newValidity }, Cmd.none )

    RunHttpRequest ->
      let
        httpRequest = Http.request
          { method = Builder.Method.toString model.method
          , headers = List.map Builder.Header.mkHeader model.headers
          , url = Builder.Url.fullUrl model
          , body = Http.emptyBody
          , expect = Http.expectString GetHttpResponse
          , timeout = Nothing
          , tracker = Nothing
          }
      in (model, httpRequest)

    SetHttpBody body ->
      ( { model | body = body }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  div [ id "builder" ]
    [ Builder.Url.view model
    , Builder.Header.view model
    , Builder.Body.view model
    , Builder.Response.view model
    ]
