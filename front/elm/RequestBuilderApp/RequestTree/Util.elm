module RequestBuilderApp.RequestTree.Util exposing (..)

import Application.Type exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import Animation


-- * find


findNode : List RequestNode -> Uuid -> Maybe RequestNode
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
    List.head <| catMaybes (List.map find requestNodes)


findFile : List RequestNode -> Uuid -> Maybe RequestFileRecord
findFile requestNodes id =
    case findNode requestNodes id of
        Just (RequestFile file) ->
            Just file

        _ ->
            Nothing

getRequestNodeId : RequestNode -> Uuid
getRequestNodeId requestNode =
    case requestNode of
        RequestFolder { id } ->
            id

        RequestFile { id } ->
            id


-- * modify


modifyRequestNode : Uuid -> (RequestNode -> RequestNode) -> RequestNode -> RequestNode
modifyRequestNode id f requestNode =
    case getRequestNodeId requestNode == id of
        True ->
            f requestNode

        False ->
            case requestNode of
                RequestFile requestFile ->
                    RequestFile requestFile

                RequestFolder requestFolder ->
                    RequestFolder
                        { requestFolder
                            | children =
                                List.map (modifyRequestNode id f) requestFolder.children
                        }

-- * delete


deleteRequestNode : Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getRequestNodeId requestNode == idToDelete of
        True ->
            []

        False ->
            case requestNode of
                RequestFile requestFile ->
                    [ RequestFile requestFile ]

                RequestFolder requestFolder ->
                    [ RequestFolder
                        { requestFolder
                            | children =
                                List.concatMap (deleteRequestNode idToDelete) requestFolder.children
                        }
                    ]


-- * toggle


toggleFolder : RequestNode -> RequestNode
toggleFolder node =
    case node of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | open = not folder.open
                }


-- * mkdir


mkdir : Uuid -> RequestNode -> RequestNode
mkdir id node =
    case node of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | children = mkDefaultFolder id :: folder.children
                    , open = True
                }

-- * touch


touch : Uuid -> RequestNode -> RequestNode
touch id parentNode =
    case parentNode of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | children = mkDefaultFile id :: folder.children
                    , open = True
                }

-- * display rename input


displayRenameInput : RequestNode -> RequestNode
displayRenameInput node =
    case node of
        RequestFolder folder ->
            let
                oldValue =
                    notEditedValue folder.name
            in
            RequestFolder { folder | name = Edited oldValue oldValue }

        RequestFile file ->
            let
                oldValue =
                    notEditedValue file.name
            in
            RequestFile { file | name = Edited oldValue oldValue }

-- * rename

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


-- * mk default


mkDefaultFolder : Uuid -> RequestNode
mkDefaultFolder id =
    RequestFolder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = []
        }

mkDefaultFile : Uuid -> RequestNode
mkDefaultFile id =
    RequestFile
        { id = id
        , name = NotEdited "new request"
        , httpUrl = NotEdited ""
        , httpMethod = NotEdited HttpGet
        , httpHeaders = NotEdited []
        , httpBody = NotEdited ""
        , showResponseView = False
        , whichResponseView = BodyResponseView
        , requestComputationResult = Nothing
        , runRequestIconAnimation = Animation.style []
        }
