module BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.BuilderTree.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)

import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Model as Builder

findNode : BuilderTree -> Int -> Maybe Node
findNode =
  let
    find : BuilderTree -> Int -> (Int, Maybe Node)
    find tree idx =
      case (tree, idx) of
        (node :: tail, 0) -> (0, Just node)
        ([], _) -> (idx, Nothing)
        (node :: tail, _) ->
           case node of
             Folder { children }  ->
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

findBuilder : BuilderTree -> Int -> Maybe Builder.Model
findBuilder tree idx =
  case findNode tree idx of
    Just (File { builder }) -> Just builder
    _ -> Nothing

modifyNode : (Node -> Node) -> BuilderTree -> Int -> BuilderTree
modifyNode f =
  let
    modify : BuilderTree -> Int -> (Int, BuilderTree)
    modify tree idx =
      if idx < 0 then
        (idx, tree)
      else
        case (tree, idx) of
          (node :: tail, 0) -> (-1, (f node) :: tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               Folder { name, open, showRenameInput, children } ->
                 let
                    (newIdx, newChildren) = modify children (idx - 1)
                    (rIdx, newTail) = modify tail newIdx
                    newFolder = Folder { name = name, open = open, showRenameInput = showRenameInput, children = newChildren }
                 in (rIdx, newFolder :: newTail)

               _ ->
                 let
                   (newIdx, newBuilderTree) = (modify tail (idx - 1))
                 in
                   (newIdx, node :: newBuilderTree)
  in
    \x y -> modify x y |> Tuple.second

deleteNode : BuilderTree -> Int -> BuilderTree
deleteNode =
  let
    delete : BuilderTree -> Int -> (Int, BuilderTree)
    delete tree idx =
      if idx < 0 then
        (idx, tree)
      else
        case (tree, idx) of
          (node :: tail, 0) -> (-1, tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               Folder { name, open, showRenameInput, children } ->
                 let
                    (newIdx, newChildren) = delete children (idx - 1)
                    (rIdx, newTail) = delete tail newIdx
                    newFolder = Folder { name = name
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
    File _ as file -> file
    Folder folder ->
        Folder { folder
                   | open = (not folder.open)
                   , showRenameInput = False
               }

mkdir : Node -> Node
mkdir node =
  case node of
    File _ as file -> file
    Folder folder ->
        Folder { folder | children = defaultFolder :: folder.children
               , showRenameInput = False
               }

touch : Node -> Node
touch node =
  case node of
    File _ as file -> file
    Folder folder ->
      Folder { folder | children = (defaultFile :: folder.children)
             , showRenameInput = False
             }

displayRenameInput : Node -> Node
displayRenameInput node =
  case node of
    Folder folder -> Folder { folder | showRenameInput = True }
    File file -> File { file | showRenameInput = True }

rename : String -> Node -> Node
rename newName node =
  case node of
    Folder folder -> Folder { folder | name = newName, showRenameInput = False }
    File file -> File { file | name = newName, showRenameInput = False }
