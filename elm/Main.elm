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

type alias Builders = List(Builder.Model)
type Tree = Folder String Tree | Files Builders

type alias Model =
  { selectedNode : Builder.Model
  , tree : Tree
  }

type Msg
  = SetBuilder Builder.Model
  | BuilderMsg Builder.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    model =
      { selectedNode = Builder.defaultModel1
      , tree = Folder "ibbu" (Files [Builder.defaultModel1, Builder.defaultModel2])
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetBuilder builder ->
      ( { model | selectedNode = builder }, Cmd.none )
    BuilderMsg subMsg ->
      let
        (updatedBuilder, builderCmd) = Builder.update subMsg model.selectedNode
      in
        ( { model | selectedNode = updatedBuilder }, Cmd.map BuilderMsg builderCmd )

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
        , div [] [ builderAppView model.selectedNode ]
        ]
  in
    div [ id "app" ] [ builderView ]

treeView : Model -> Html Msg
treeView model =
  let
    entryView : Tree -> Html Msg
    entryView tree =
      case tree of
        Folder name content ->
          div []
            [ b [] [ text name ]
            , ol [] [ entryView content ]
            ]

        Files builders ->
          let
            fileView builder = li [ onClick (SetBuilder builder) ] [ text builder.name ]
          in
            ol [] (List.map fileView builders)
  in
    ol [] [ entryView model.tree ]

builderAppView : Builder.Model -> Html Msg
builderAppView builder =
  Html.map BuilderMsg (Builder.view builder)
