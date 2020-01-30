module BuilderApp.BuilderTree.Util exposing (..)

--import BuilderApp.BuilderTree.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)

--import BuilderApp.Builder.App as Builder
import Application.Type exposing (..)
import BuilderApp.Model exposing (..)

findRequestNode : List RequestNode -> Int -> Maybe RequestNode
findRequestNode =
  let
    find : List RequestNode -> Int -> (Int, Maybe RequestNode)
    find requestCollection idx =
      case (requestCollection, idx) of
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

findFile : List RequestNode -> Int -> Maybe BuilderApp.Model.File
findFile requestCollection idx =
    case findRequestNode requestCollection idx of
        Just (RequestFile file) -> Just file
        _ -> Nothing

modifyRequestNode : (RequestNode -> RequestNode) -> List RequestNode -> Int -> List RequestNode
modifyRequestNode f =
  let
    modify : List RequestNode -> Int -> (Int, List RequestNode)
    modify requestCollection idx =
      if idx < 0 then
        (idx, requestCollection)
      else
        case (requestCollection, idx) of
          (node :: tail, 0) -> (-1, (f node) :: tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               RequestFolder { id, name, open, children } ->
                 let
                    (newIdx, newChildren) = modify children (idx - 1)
                    (rIdx, newTail) = modify tail newIdx
                    newFolder =
                        RequestFolder { id = id
                                      , name = name
                                      , open = open
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

deleteRequestNode : List RequestNode -> Int -> List RequestNode
deleteRequestNode =
  let
    delete : List RequestNode -> Int -> (Int, List RequestNode)
    delete requestCollection idx =
      if idx < 0 then
        (idx, requestCollection)
      else
        case (requestCollection, idx) of
          (node :: tail, 0) -> (-1, tail)
          ([], _) -> (idx, [])
          (node :: tail, _) ->
             case node of
               RequestFolder { id, name, open, children } ->
                 let
                    (newIdx, newChildren) = delete children (idx - 1)
                    (rIdx, newTail) = delete tail newIdx
                    newFolder = RequestFolder { id = id
                                              , name = name
                                              , open = open
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

toggleFolder : RequestNode -> RequestNode
toggleFolder node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | open = (not folder.open)
                      }

mkdir : RequestNode -> RequestNode
mkdir node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | children = defaultFolder :: folder.children
                      }

touch : RequestNode -> RequestNode
touch node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
      RequestFolder { folder
                        | children = (defaultFile :: folder.children)
                    }

displayRenameInput : RequestNode -> RequestNode
displayRenameInput node =
  case node of
    RequestFolder folder ->
        let
            oldValue = notEditedValue folder.name
        in
            RequestFolder { folder | name = Edited oldValue oldValue }

    RequestFile file ->
        let
            oldValue = notEditedValue file.name
        in
            RequestFile { file | name = Edited oldValue oldValue }

rename : String -> RequestNode -> RequestNode
rename newName node =
  case node of
    RequestFolder folder ->
        RequestFolder { folder | name = NotEdited newName }
    RequestFile file ->
        RequestFile { file | name = NotEdited newName }

tempRename : String -> RequestNode -> RequestNode
tempRename newName node =
    case node of
        RequestFolder folder ->
            RequestFolder { folder | name = changeEditedValue newName folder.name }

        RequestFile file ->
            RequestFile { file | name = changeEditedValue newName file.name }
