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
import Builder.Header
import Builder.Url
import Builder.Body
import Builder.Tree
import Builder.App as BuilderApp
import Builder.Model

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model = List ((Builder.Model.Model, Bool))
type Msg = SetBuilder Builder.Model.Model

init : () -> (Model, Cmd Msg)
init _ =
  let
    model = [ (BuilderApp.defaultModel, True) ]
  in (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg builders =
  case msg of
    SetBuilder builder -> (builders, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view builders =
  let
    builderView : (Builder.Model.Model, Bool) -> Html Msg
    builderView (builder, selected) =
      div []
        [ div [ onClick (SetBuilder builder) ] [ text builder.name ]
        ]
  in
    div [ id "app" ] (List.map builderView builders)
