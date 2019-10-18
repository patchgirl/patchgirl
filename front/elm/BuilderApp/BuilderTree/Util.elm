module BuilderApp.BuilderTree.Util exposing (..)

--import BuilderApp.BuilderTree.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)

--import BuilderApp.Builder.App as Builder
import BuilderApp.Model exposing (..)
import BuilderApp.Builder.Model as Builder

findNode : List Node -> Int -> Maybe Node
findNode =
  let
    find : List Node -> Int -> (Int, Maybe Node)
    find tree idx =
      case (tree, idx) of
        (node :: tail, 0) -> (0, Just node)
        ([], _) -> (idx, Nothing)
        (node :: tail, _) ->
           case node of
             RequestFolder { children }  ->
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

findFile : List Node -> Int -> Maybe BuilderApp.Model.File
findFile tree idx =
    case findNode tree idx of
        Just (RequestFile file) -> Just file
        _ -> Nothing

modifyNode : (Node -> Node) -> List Node -> Int -> List Node
modifyNode f =
  let
    modify : List Node -> Int -> (Int, List Node)
    modify tree idx =
      if idx < 0 then
        (idx, tree)
      else
        case (tree, idx) of
          (node :: tail, 0) -> (-1, (f node) :: tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               RequestFolder { name, open, showRenameInput, children } ->
                 let
                    (newIdx, newChildren) = modify children (idx - 1)
                    (rIdx, newTail) = modify tail newIdx
                    newFolder =
                        RequestFolder { name = name
                                      , open = open
                                      , showRenameInput = showRenameInput
                                      , children = newChildren
                                      }
                 in (rIdx, newFolder :: newTail)

               _ ->
                 let
                   (newIdx, newBuilderTree) = (modify tail (idx - 1))
                 in
                   (newIdx, node :: newBuilderTree)
  in
    \x y -> modify x y |> Tuple.second

deleteNode : List Node -> Int -> List Node
deleteNode =
  let
    delete : List Node -> Int -> (Int, List Node)
    delete tree idx =
      if idx < 0 then
        (idx, tree)
      else
        case (tree, idx) of
          (node :: tail, 0) -> (-1, tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               RequestFolder { name, open, showRenameInput, children } ->
                 let
                    (newIdx, newChildren) = delete children (idx - 1)
                    (rIdx, newTail) = delete tail newIdx
                    newFolder = RequestFolder { name = name
                                              , open = open
                                              , showRenameInput = showRenameInput
                                              , children = newChildren
                                              }
                 in (rIdx, newFolder :: newTail)

               _ ->
                 let
                   (newIdx, newBuilderTree) = (delete tail (idx - 1))
                 in
                   (newIdx, node :: newBuilderTree)
  in
    \x y -> delete x y |> Tuple.second

toggleFolder : Node -> Node
toggleFolder node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | open = (not folder.open)
                          , showRenameInput = False
                      }

mkdir : Node -> Node
mkdir node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | children = defaultFolder :: folder.children
                          , showRenameInput = False
                      }

touch : Node -> Node
touch node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
      RequestFolder { folder
                        | children = (defaultFile :: folder.children)
                        , showRenameInput = False
                    }

displayRenameInput : Node -> Node
displayRenameInput node =
  case node of
    RequestFolder folder ->
        RequestFolder { folder | showRenameInput = True }
    RequestFile file ->
        RequestFile { file | showRenameInput = True }

rename : String -> Node -> Node
rename newName node =
  case node of
    RequestFolder folder ->
        RequestFolder { folder
                          | name = newName
                          , showRenameInput = False
                      }
    RequestFile file ->
        RequestFile { file
                        | name = newName
                        , showRenameInput = False
                    }
