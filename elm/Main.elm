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

type Node = Folder String Bool Tree | File String Builder.Model
type alias Tree = List(Node)

type alias Model =
  { selectedNode : Maybe Node
  , displayedBuilderIndex : Maybe Int
  , tree : Tree
  }

type Msg
  = SetSelectedNode Node
  | SetDisplayedBuilder Int
  | ToggleNode Int
  | BuilderMsg Builder.Msg

init : () -> (Model, Cmd Msg)
init _ =
  let
    model =
      { selectedNode = Nothing
      , displayedBuilderIndex = Nothing
      , tree = [ Folder "folder1" False []
               , Folder "folder2" True [ Folder "folder2.2" True [] ]
               , Folder "folder3" True <| [ File "file1" Builder.defaultModel1
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
        mBuilder : Maybe Builder.Model
        mBuilder = model.displayedBuilderIndex |> Maybe.andThen (findBuilder model.tree)
        mUpdatedBuilderToCmd : Maybe (Builder.Model, Cmd Builder.Msg)
        mUpdatedBuilderToCmd = Maybe.map (Builder.update subMsg) mBuilder
      in
        case mUpdatedBuilderToCmd of
          Just(updatedBuilder, builderCmd) -> ( model, Cmd.map BuilderMsg builderCmd )
          Nothing -> (model, Cmd.none)

    SetDisplayedBuilder idx ->
      ( { model | displayedBuilderIndex = Just idx }, Cmd.none)

    ToggleNode idx ->
      let
        toggle : Node -> Node
        toggle node =
          case node of
            File _ _ as file -> file
            Folder name toggleState children as folder ->
              Folder name (not toggleState) children
      in
        ( { model | tree = (modifyNode toggle model.tree idx) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model =
  let
    builderView : Html Msg
    builderView =
      div []
        [ div [] [ treeView model.tree ]
        , div [] [ builderAppView model ]
        ]
  in
    div [ id "app" ] [ builderView ]

builderAppView : Model -> Html Msg
builderAppView model =
  model.displayedBuilderIndex
    |> Maybe.andThen (findBuilder model.tree)
    |> Maybe.map Builder.view
    |> Maybe.map (Html.map BuilderMsg)
    |> Maybe.withDefault (div [] [ text (Maybe.withDefault "nope" (Maybe.map String.fromInt(model.displayedBuilderIndex))) ])

treeView : Tree -> Html Msg
treeView tree =
  ul [] <| Tuple.second (nodeView 0 tree)

findNode : Tree -> Int -> Maybe Node
findNode =
  let
    find : Tree -> Int -> (Int, Maybe Node)
    find tree idx =
      case (tree, idx) of
        (node :: tail, 0) -> (0, Just node)
        ([], _) -> (idx, Nothing)
        (node :: tail, _) ->
           case node of
             Folder _ _ children  ->
               let
                  (newIdx, folderSearch) = find children (idx - 1)
                  (_, tailSearch) = find tail newIdx
               in
                 case (folderSearch, tailSearch) of
                   (Just n, _) -> (0, Just n)
                   (_, Just n) -> (0, Just n)
                   _ -> (newIdx, Nothing)

             _ -> find tail (idx - 1)
  in
    \x y -> find x y |> Tuple.second

nodeView : Int -> Tree -> (Int, List (Html Msg))
nodeView idx tree =
  case tree of
    [] -> (idx, [])
    node :: tail ->
      case node of
        Folder name open children ->
          let
            (folderIdx, folderChildrenView) = nodeView (idx + 1) children
            (newIdx, tailView) = nodeView folderIdx tail
            folderToggleView = if open then "-" else "+"
            folderView = li [] [ b [ onClick (ToggleNode idx) ] [ text (folderToggleView ++ " " ++ name ++ "/") ]
                               , ul [ hidden (not open) ] folderChildrenView
                               ]
          in
            (newIdx, folderView :: tailView)

        File name _ ->
          let
            (newIdx, tailView) = nodeView (idx + 1) tail
            fileView = li [ onClick (SetDisplayedBuilder idx) ] [ text name ]
          in
            (newIdx, fileView :: tailView)

findBuilder : Tree -> Int -> Maybe Builder.Model
findBuilder tree idx =
  case findNode tree idx of
    Just (File _ builder) -> Just builder
    _ -> Nothing

modifyNode : (Node -> Node) -> Tree -> Int -> Tree
modifyNode f =
  let
    modify : Tree -> Int -> (Int, Tree)
    modify tree idx =
      if idx < 0 then
        (idx, tree)
      else
        case (tree, idx) of
          (node :: tail, 0) -> (-1, (f node) :: tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               Folder name open children  ->
                 let
                    (newIdx, newChildren) = modify children (idx - 1)
                    (rIdx, newTail) = modify tail newIdx
                    newFolder = Folder name open newChildren
                 in (rIdx, newFolder :: newTail)

               _ ->
                 let
                   (newIdx, newTree) = (modify tail (idx - 1))
                 in
                   (newIdx, node :: newTree)
  in
    \x y -> modify x y |> Tuple.second
