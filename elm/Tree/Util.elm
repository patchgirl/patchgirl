module Tree.Util exposing (..)

import Tree.Model exposing (..)
import Tree.Message exposing (..)

import Builder.App as Builder
import Builder.Model as Builder

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

findBuilder : Tree -> Int -> Maybe Builder.Model
findBuilder tree idx =
  case findNode tree idx of
    Just (File { builder }) -> Just builder
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
               Folder { name, open, showRenameInput, children } ->
                 let
                    (newIdx, newChildren) = modify children (idx - 1)
                    (rIdx, newTail) = modify tail newIdx
                    newFolder = Folder { name = name, open = open, showRenameInput = showRenameInput, children = newChildren }
                 in (rIdx, newFolder :: newTail)

               _ ->
                 let
                   (newIdx, newTree) = (modify tail (idx - 1))
                 in
                   (newIdx, node :: newTree)
  in
    \x y -> modify x y |> Tuple.second

deleteNode : Tree -> Int -> Tree
deleteNode =
  let
    delete : Tree -> Int -> (Int, Tree)
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
                   (newIdx, newTree) = (delete tail (idx - 1))
                 in
                   (newIdx, node :: newTree)
  in
    \x y -> delete x y |> Tuple.second
