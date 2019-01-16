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

type Node = Folder String Tree | File String Builder.Model
type alias Tree = List(Node)

type alias Model =
  { selectedNode : Maybe Node
  , displayedBuilder : Maybe Builder.Model
  , tree : Tree
  }

type Msg
  = SetSelectedNode Node
  | SetDisplayedBuilder Builder.Model
  | BuilderMsg Builder.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    model =
      { selectedNode = Nothing
      , displayedBuilder = Nothing
      , tree = [ Folder "folder1" []
               , Folder "folder2" [ Folder "folder2.2" [] ]
               , Folder "folder3" <| [ File "file1" Builder.defaultModel1
                                     , File "file2" Builder.defaultModel2
                                     ]
               ]
      }
  in
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetSelectedNode node ->
      ( { model | selectedNode = Just node }, Cmd.none )

    BuilderMsg subMsg ->
      let
        mUpdatedBuilderToBuilderCmd = Maybe.map (Builder.update subMsg) model.displayedBuilder
      in
        case mUpdatedBuilderToBuilderCmd of
          Just(updatedBuilder, builderCmd) ->
            ( { model | displayedBuilder = Just(updatedBuilder) }, Cmd.map BuilderMsg builderCmd )
          Nothing -> (model, Cmd.none)

    SetDisplayedBuilder builder ->
      ( { model | displayedBuilder = Just builder }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] (treeView model.tree)
        , div [] [ builderAppView model.displayedBuilder ]
        ]
  in
    div [ id "app" ] [ builderView ]

treeView : Tree -> List(Html Msg)
treeView tree =
  let
    nodeView : Node -> Html Msg
    nodeView node =
      case node of
        Folder name children ->
          div []
            [ b [] [ text name ]
            , ol [] (treeView children)
            ]

        File name builder ->
          li [ onClick (SetDisplayedBuilder builder) ] [ text name ]
  in
    List.map nodeView tree

builderAppView : Maybe Builder.Model -> Html Msg
builderAppView mBuilder =
  case mBuilder of
    Just builder -> Html.map BuilderMsg (Builder.view builder)
    Nothing -> div [] []
