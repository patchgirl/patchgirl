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
                (File file) as node ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (Folder folder) as node ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            let
                                (Children children) =
                                    folder.children
                            in
                            findNode children id
    in
    List.head <| catMaybes (List.map find requestNodes)


findFile : List RequestNode -> Uuid -> Maybe RequestFileRecord
findFile requestNodes id =
    case findNode requestNodes id of
        Just (File file) ->
            Just file

        _ ->
            Nothing

findFolder : List RequestNode -> Uuid -> Maybe RequestFolderRecord
findFolder requestNodes id =
    case findNode requestNodes id of
        Just (Folder folder) ->
            Just folder

        _ ->
            Nothing

getRequestNodeId : RequestNode -> Uuid
getRequestNodeId requestNode =
    case requestNode of
        Folder { id } ->
            id

        File { id } ->
            id


-- * modify


modifyRequestNode : Uuid -> (RequestNode -> RequestNode) -> RequestNode -> RequestNode
modifyRequestNode id f requestNode =
    case getRequestNodeId requestNode == id of
        True ->
            f requestNode

        False ->
            case requestNode of
                File requestFile ->
                    File requestFile

                Folder folder ->
                    let
                        (Children children) =
                            folder.children
                    in
                    Folder
                        { folder
                            | children =
                                Children (List.map (modifyRequestNode id f) children)
                        }

-- * delete


deleteRequestNode : Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getRequestNodeId requestNode == idToDelete of
        True ->
            []

        False ->
            case requestNode of
                File requestFile ->
                    [ File requestFile ]

                Folder folder ->
                    let
                        (Children children) =
                            folder.children
                    in
                    [ Folder
                        { folder
                            | children =
                                Children (List.concatMap (deleteRequestNode idToDelete) children)
                        }
                    ]


-- * toggle


toggleFolder : RequestNode -> RequestNode
toggleFolder node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder
                { folder
                    | open = not folder.open
                }


-- * mkdir


mkdir : Uuid -> RequestNode -> RequestNode
mkdir id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (Children children) =
                    folder.children
            in
            Folder
                { folder
                    | children = Children (mkDefaultFolder id :: children)
                    , open = True
                }

-- * touch


touch : Uuid -> RequestNode -> RequestNode
touch id parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            let
                (Children children) =
                    folder.children
            in
            Folder
                { folder
                    | children = Children (mkDefaultFile id :: children)
                    , open = True
                }

-- * display rename input


displayRenameInput : RequestNode -> RequestNode
displayRenameInput node =
    case node of
        Folder folder ->
            let
                oldValue =
                    notEditedValue folder.name
            in
            Folder { folder | name = Edited oldValue oldValue }

        File file ->
            let
                oldValue =
                    notEditedValue file.name
            in
            File { file | name = Edited oldValue oldValue }

-- * rename


rename : String -> RequestNode -> RequestNode
rename newName node =
    case node of
        Folder folder ->
            Folder { folder | name = NotEdited newName }

        File file ->
            File { file | name = NotEdited newName }


tempRename : String -> RequestNode -> RequestNode
tempRename newName node =
    case node of
        Folder folder ->
            Folder { folder | name = changeEditedValue newName folder.name }

        File file ->
            File { file | name = changeEditedValue newName file.name }


-- * mk default


mkDefaultFolder : Uuid -> RequestNode
mkDefaultFolder id =
    Folder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = Children []
        }

mkDefaultFile : Uuid -> RequestNode
mkDefaultFile id =
    File
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
