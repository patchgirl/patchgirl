import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra as L

import Builder.Message as Builder
import Builder.App as Builder
import Builder.Model as Builder

import Tree.View as Tree
import Tree.Model as Tree
import Tree.Message as Tree
import Tree.App as Tree

import Postman.View as Postman
import Postman.Model as Postman
import Postman.Message as Postman
import Postman.App as Postman

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { treeModel : Tree.Model
  , postmanModel : Postman.Model
  }

type Msg
  = TreeMsg Tree.Msg
  | BuilderMsg Builder.Msg
  | PostmanMsg Postman.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    treeModel =
      { selectedNode = Nothing
      , displayedBuilderIndex = Nothing
      , tree = [ Tree.Folder "folder1" False []
               , Tree.Folder "folder2" True [ Tree.Folder "folder2.2" True [] ]
               , Tree.Folder "folder3" True <| [ Tree.File "file1" Builder.defaultModel1
                                               , Tree.File "file2" Builder.defaultModel2
                                               ]
               ]
      }
    model =
      { treeModel = treeModel
      , postmanModel = "init"
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TreeMsg subMsg ->
      case Tree.update subMsg model.treeModel of
        (newTreeModel, newMsg) -> ( { model | treeModel = newTreeModel }, Cmd.map TreeMsg newMsg)

    PostmanMsg subMsg ->
      case Postman.update subMsg model.postmanModel of
        (newPostmanModel, newMsg) -> ( { model | postmanModel = newPostmanModel }, Cmd.map PostmanMsg newMsg)

    BuilderMsg subMsg ->
      let
        mBuilder : Maybe Builder.Model
        mBuilder = model.treeModel.displayedBuilderIndex |> Maybe.andThen (Tree.findBuilder model.treeModel.tree)
        mUpdatedBuilderToCmd : Maybe (Builder.Model, Cmd Builder.Msg)
        mUpdatedBuilderToCmd = Maybe.map (Builder.update subMsg) mBuilder
      in
        case mUpdatedBuilderToCmd of
          Just(updatedBuilder, builderCmd) -> ( model, Cmd.map BuilderMsg builderCmd )
          Nothing -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ postmanView model.postmanModel ]
        , div [] [ treeView model ]
        , div [] [ builderAppView model.treeModel ]
        ]
  in
    div [ id "app" ] [ builderView ]

postmanView : Postman.Model -> Html Msg
postmanView postmanModel =
  Html.map PostmanMsg (Postman.view postmanModel)

builderAppView : Tree.Model -> Html Msg
builderAppView treeModel =
  treeModel.displayedBuilderIndex
    |> Maybe.andThen (Tree.findBuilder treeModel.tree)
    |> Maybe.map Builder.view
    |> Maybe.map (Html.map BuilderMsg)
    |> Maybe.withDefault (div [] [ text (Maybe.withDefault "nope" (Maybe.map String.fromInt(treeModel.displayedBuilderIndex))) ])

treeView : Model -> Html Msg
treeView model =
  Html.map TreeMsg (Tree.view model.treeModel.tree)
