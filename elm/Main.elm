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

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Msg
  = TreeMsg Tree.Msg
  | BuilderMsg Builder.Msg

init : () -> (Tree.Model, Cmd Msg)
init _ =
  let
    model =
      { selectedNode = Nothing
      , displayedBuilderIndex = Nothing
      , tree = [ Tree.Folder "folder1" False []
               , Tree.Folder "folder2" True [ Tree.Folder "folder2.2" True [] ]
               , Tree.Folder "folder3" True <| [ Tree.File "file1" Builder.defaultModel1
                                               , Tree.File "file2" Builder.defaultModel2
                                               ]
               ]
      }
  in
    (model, Cmd.none)

update : Msg -> Tree.Model -> (Tree.Model, Cmd Msg)
update msg model =
  case msg of
    TreeMsg subMsg ->
      case Tree.update subMsg model of
        (newModel, newMsg) -> ( newModel, Cmd.map TreeMsg newMsg)

    BuilderMsg subMsg ->
      let
        mBuilder : Maybe Builder.Model
        mBuilder = model.displayedBuilderIndex |> Maybe.andThen (Tree.findBuilder model.tree)
        mUpdatedBuilderToCmd : Maybe (Builder.Model, Cmd Builder.Msg)
        mUpdatedBuilderToCmd = Maybe.map (Builder.update subMsg) mBuilder
      in
        case mUpdatedBuilderToCmd of
          Just(updatedBuilder, builderCmd) -> ( model, Cmd.map BuilderMsg builderCmd )
          Nothing -> (model, Cmd.none)

subscriptions : Tree.Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Tree.Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ treeView model ]
        , div [] [ builderAppView model ]
        ]
  in
    div [ id "app" ] [ builderView ]

builderAppView : Tree.Model -> Html Msg
builderAppView model =
  model.displayedBuilderIndex
    |> Maybe.andThen (Tree.findBuilder model.tree)
    |> Maybe.map Builder.view
    |> Maybe.map (Html.map BuilderMsg)
    |> Maybe.withDefault (div [] [ text (Maybe.withDefault "nope" (Maybe.map String.fromInt(model.displayedBuilderIndex))) ])

treeView : Tree.Model -> Html Msg
treeView model =
  Html.map TreeMsg (Tree.view model.tree)
