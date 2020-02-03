module BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.Model as Builder
import BuilderApp.BuilderTree.Message exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe
import Util.Maybe as Maybe
import Uuid
import Application.Type exposing (..)
import BuilderApp.Model exposing (..)

findFile : List RequestNode -> Uuid.Uuid -> Maybe BuilderApp.Model.File
findFile requestNodes id =
    case findNode requestNodes id of
        Just (RequestFile file) ->
            Just file

        _ ->
            Nothing

findNode : List RequestNode -> Uuid.Uuid -> Maybe RequestNode
findNode requestNodes id =
    let
        find : RequestNode -> Maybe RequestNode
        find requestNode =
            case requestNode of
                (RequestFile file) as node ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (RequestFolder folder) as node ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            findNode folder.children id
    in
        List.head <| Maybe.catMaybes (List.map find requestNodes)



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
