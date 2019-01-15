import Browser
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, href, disabled, class, id)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import Debug
import Url
import Json.Decode as Json
import List.Extra as L

import Builder.Response
import Builder.Message as BuilderMsg
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

type alias Model =
  { selectedBuilder : Builder.Model.Model
  , builders : List Builder.Model.Model
  }

type Msg
  = SetBuilder Builder.Model.Model
  | BuilderMsg BuilderMsg.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    model =
      { selectedBuilder = BuilderApp.defaultModel1
      , builders = [BuilderApp.defaultModel1, BuilderApp.defaultModel2]
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetBuilder builder ->
      ( { model | selectedBuilder = builder }, Cmd.none )
    BuilderMsg subMsg ->
      let
        (updatedBuilder, builderCmd) = BuilderApp.update subMsg model.selectedBuilder
      in
        ( { model | selectedBuilder = updatedBuilder }, Cmd.map BuilderMsg builderCmd )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ treeView model ]
        , div [] [ builderAppView model.selectedBuilder ]
        ]
  in
    div [ id "app" ] [ builderView ]

treeView : Model -> Html Msg
treeView model =
  let
    entryView : Builder.Model.Model -> Html Msg
    entryView builder =
      li [ onClick (SetBuilder builder) ] [ text builder.name ]
  in
    ol [] (List.map entryView model.builders)

builderAppView : Builder.Model.Model -> Html Msg
builderAppView builder =
  Html.map BuilderMsg (BuilderApp.view builder)
