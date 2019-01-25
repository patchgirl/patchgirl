module Builder.App exposing (..)

import Browser
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
import Builder.Method as Builder

defaultModel1 =
  { name = "no name"
  , url = "swapi.co/api/people/1"
  , scheme = "HTTP"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel2 =
  { name = "no name2"
  , url = "swapi.co/api/people/2"
  , scheme = "HTTP"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateUrl url ->
      ( { model | url = url }, Cmd.none)

    SetHttpMethod newMethod ->
      case newMethod of
        "GET" -> ( { model | method = Get }, Cmd.none)
        _ -> ( { model | method = Post }, Cmd.none)

    SetHttpScheme scheme ->
      case scheme of
        "HTTP" -> ( { model | scheme = "HTTP" }, Cmd.none)
        _ -> ( { model | scheme = "HTTPS" }, Cmd.none)

    AskRun ->
      (model, Cmd.none)

    GiveResponse result ->
      ( { model | response = Just result }, Cmd.none)

    UpdateHeaders rawHeaders ->
      case Builder.Header.parseHeaders rawHeaders of
        Just headers ->
          ( { model | headers = headers }, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    SetHttpBody body ->
      ( { model | body = body }, Cmd.none )

view : Model -> Html Msg
view model =
  div [ id "builder" ]
    [ Builder.Url.view model
    , Builder.Header.view model
    , Builder.Body.view model
    , Builder.Response.view model
    ]
