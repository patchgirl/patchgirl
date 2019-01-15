import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra as L

import Builder.Message as Builder
import Builder.App as Builder
import Builder.Model as Builder

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { selectedBuilder : Builder.Model
  , builders : List Builder.Model
  }

type Msg
  = SetBuilder Builder.Model
  | BuilderMsg Builder.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    model =
      { selectedBuilder = Builder.defaultModel1
      , builders = [Builder.defaultModel1, Builder.defaultModel2]
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
        (updatedBuilder, builderCmd) = Builder.update subMsg model.selectedBuilder
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
    entryView : Builder.Model -> Html Msg
    entryView builder =
      li [ onClick (SetBuilder builder) ] [ text builder.name ]
  in
    ol [] (List.map entryView model.builders)

builderAppView : Builder.Model -> Html Msg
builderAppView builder =
  Html.map BuilderMsg (Builder.view builder)
