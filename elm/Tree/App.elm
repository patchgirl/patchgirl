module Tree.App exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))

import Builder.App as Builder
import Builder.Model as Builder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetSelectedNode node ->
      ( { model | selectedNode = Just node }, Cmd.none )

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

    Mkdir idx ->
      let
        mkdir : Node -> Node
        mkdir node =
          case node of
            File _ _ as file -> file
            Folder name toggleState children as folder ->
              Folder name (not toggleState) (Folder "newFolder" False [] :: children)
      in
        ( { model | tree = (modifyNode mkdir model.tree idx) }, Cmd.none)

    Touch idx ->
      let
        newFile = File "newFile" Builder.defaultModel1
        touch : Node -> Node
        touch node =
          case node of
            File _ _ as file -> file
            Folder name toggleState children as folder ->
              Folder name (not toggleState) (newFile :: children)
      in
        ( { model | tree = (modifyNode touch model.tree idx) }, Cmd.none)

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
