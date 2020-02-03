module BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.Model as Builder
import BuilderApp.BuilderTree.Message exposing (..)

import Uuid
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

modifyRequestNode2 : Uuid.Uuid -> (RequestNode -> RequestNode) -> RequestNode -> RequestNode
modifyRequestNode2 id f requestNode =
    case getRequestNodeId requestNode == id of
        True -> f requestNode
        False ->
            case requestNode of
                RequestFile requestFile ->
                    RequestFile requestFile

                RequestFolder requestFolder ->
                    RequestFolder { requestFolder
                                      | children =
                                        List.map (modifyRequestNode2 id f) requestFolder.children
                                  }


deleteRequestNode : Uuid.Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getRequestNodeId requestNode == idToDelete of
        True -> []
        False ->
            case requestNode of
                RequestFile requestFile ->
                    [RequestFile requestFile]

                RequestFolder requestFolder ->
                    [ RequestFolder { requestFolder
                                      | children =
                                        List.concatMap (deleteRequestNode idToDelete) requestFolder.children
                                  }
                    ]


toggleFolder : RequestNode -> RequestNode
toggleFolder node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | open = (not folder.open)
                      }

mkdir : Uuid.Uuid -> RequestNode -> RequestNode
mkdir id node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | children = mkDefaultFolder id :: folder.children
                          , open = True
                      }

touch : Uuid.Uuid -> RequestNode -> RequestNode
touch id parentNode =
  case parentNode of
    RequestFile _ as file -> file
    RequestFolder folder ->
      RequestFolder { folder
                        | children = mkDefaultFile id  :: folder.children
                        , open = True
                    }

touch2 : Uuid.Uuid -> RequestNode -> RequestNode
touch2 id node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
      RequestFolder { folder
                        | children = mkDefaultFile id  :: folder.children
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

mkDefaultFolder : Uuid.Uuid -> RequestNode
mkDefaultFolder id =
    RequestFolder { id = id
                  , name = NotEdited "new folder"
                  , open = False
                  , children = []
                  }

mkDefaultFile : Uuid.Uuid -> RequestNode
mkDefaultFile id =
    RequestFile { id = id
                , name = NotEdited "new request"
                , isSaved = False
                , httpUrl = NotEdited ""
                , httpMethod = NotEdited Builder.Get
                , httpHeaders = NotEdited []
                , httpBody = NotEdited ""
                , showResponseView = False
                , requestComputationResult = Nothing
                }
