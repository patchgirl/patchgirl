module BuilderUtil exposing (..)

import Application.Type exposing (..)
import Util exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Uuid exposing (Uuid)
import Animation
import Page exposing(..)


-- * tree

-- ** find


findRequestNode : List RequestNode -> Uuid -> Maybe RequestNode
findRequestNode requestNodes id =
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
                                (RequestChildren children) =
                                    folder.children
                            in
                            findRequestNode children id
    in
    List.head <| catMaybes (List.map find requestNodes)


findRequestFile : List RequestNode -> Uuid -> Maybe RequestFileRecord
findRequestFile requestNodes id =
    case findRequestNode requestNodes id of
        Just (File file) ->
            Just file

        _ ->
            Nothing

findRequestFolder : List RequestNode -> Uuid -> Maybe RequestFolderRecord
findRequestFolder requestNodes id =
    case findRequestNode requestNodes id of
        Just (Folder folder) ->
            Just folder

        _ ->
            Nothing


findPgNode : List PgNode -> Uuid -> Maybe PgNode
findPgNode pgNodes id =
    let
        find : PgNode -> Maybe PgNode
        find pgNode =
            case pgNode of
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
                                (PgChildren children) =
                                    folder.children
                            in
                            findPgNode children id
    in
    List.head <| catMaybes (List.map find pgNodes)


findPgFile : List PgNode -> Uuid -> Maybe PgFileRecord
findPgFile pgNodes id =
    case findPgNode pgNodes id of
        Just (File file) ->
            Just file

        _ ->
            Nothing

findPgFolder : List PgNode -> Uuid -> Maybe PgFolderRecord
findPgFolder pgNodes id =
    case findPgNode pgNodes id of
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

getPgNodeId : PgNode -> Uuid
getPgNodeId node =
    case node of
        Folder { id } ->
            id

        File { id } ->
            id


-- ** modify


modifyPgNode : Uuid -> (PgNode -> PgNode) -> PgNode -> PgNode
modifyPgNode id f requestNode =
    case getPgNodeId requestNode == id of
        True ->
            f requestNode

        False ->
            case requestNode of
                File requestFile ->
                    File requestFile

                Folder folder ->
                    let
                        (PgChildren children) =
                            folder.children
                    in
                    Folder
                        { folder
                            | children =
                                PgChildren (List.map (modifyPgNode id f) children)
                        }

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
                        (RequestChildren children) =
                            folder.children
                    in
                    Folder
                        { folder
                            | children =
                                RequestChildren (List.map (modifyRequestNode id f) children)
                        }

-- ** delete


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
                        (RequestChildren children) =
                            folder.children
                    in
                    [ Folder
                        { folder
                            | children =
                                RequestChildren (List.concatMap (deleteRequestNode idToDelete) children)
                        }
                    ]

deletePgNode : Uuid -> PgNode -> List PgNode
deletePgNode idToDelete pgNode =
    case getPgNodeId pgNode == idToDelete of
        True ->
            []

        False ->
            case pgNode of
                File pgFile ->
                    [ File pgFile ]

                Folder folder ->
                    let
                        (PgChildren children) =
                            folder.children
                    in
                    [ Folder
                        { folder
                            | children =
                                PgChildren (List.concatMap (deletePgNode idToDelete) children)
                        }
                    ]


-- ** toggle


toggleRequestFolder : RequestNode -> RequestNode
toggleRequestFolder node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder
                { folder
                    | open = not folder.open
                }

togglePgFolder : PgNode -> PgNode
togglePgFolder node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder
                { folder
                    | open = not folder.open
                }


-- ** mkdir


mkdirRequest : Uuid -> RequestNode -> RequestNode
mkdirRequest id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (RequestChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = RequestChildren (mkDefaultRequestFolder id :: children)
                    , open = True
                }

mkdirPg : Uuid -> PgNode -> PgNode
mkdirPg id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (PgChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = PgChildren (mkDefaultPgFolder id :: children)
                    , open = True
                }

-- ** touch


touchRequest : Uuid -> RequestNode -> RequestNode
touchRequest id parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            let
                (RequestChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = RequestChildren (mkDefaultRequestFile id :: children)
                    , open = True
                }

touchPg : PgNode -> PgNode -> PgNode
touchPg newNode parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            let
                (PgChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = PgChildren (newNode :: children)
                    , open = True
                }


-- ** rename


renameRequest : String -> RequestNode -> RequestNode
renameRequest newName node =
    case node of
        Folder folder ->
            Folder { folder | name = NotEdited newName }

        File file ->
            File { file | name = NotEdited newName }

renamePg : String -> PgNode -> PgNode
renamePg newName node =
    case node of
        Folder folder ->
            Folder { folder | name = NotEdited newName }

        File file ->
            File { file | name = NotEdited newName }

tempRenameRequest : String -> RequestNode -> RequestNode
tempRenameRequest newName node =
    case node of
        Folder folder ->
            Folder { folder | name = changeEditedValue newName folder.name }

        File file ->
            File { file | name = changeEditedValue newName file.name }

tempRenamePg : String -> PgNode -> PgNode
tempRenamePg newName node =
    case node of
        Folder folder ->
            Folder { folder | name = changeEditedValue newName folder.name }

        File file ->
            File { file | name = changeEditedValue newName file.name }


-- ** mk default


mkDefaultRequestFolder : Uuid -> RequestNode
mkDefaultRequestFolder id =
    Folder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = RequestChildren []
        }

mkDefaultPgFolder : Uuid -> PgNode
mkDefaultPgFolder id =
    Folder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = PgChildren []
        }

mkDefaultRequestFile : Uuid -> RequestNode
mkDefaultRequestFile id =
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

mkDefaultPgFile : Uuid -> PgNode
mkDefaultPgFile id =
    File
        { id = id
        , name = NotEdited ""
        , dbHost = NotEdited ""
        , dbPassword = NotEdited ""
        , dbPort = NotEdited ""
        , dbUser = NotEdited ""
        , dbName = NotEdited ""
        , sql = NotEdited ""
        , pgComputationOutput = Nothing
        , showResponseView = False
        }


-- * view


-- ** close builder view


closeBuilderView : Page -> Element a
closeBuilderView page =
    let
        exitLink =
            case page of
                ReqPage _ ->
                    href (ReqPage (LandingView DefaultView))
                PgPage _ ->
                    href (PgPage (LandingView DefaultView))
                _ ->
                    href (PgPage (LandingView DefaultView))
    in
    link []
        { url = exitLink
        , label =
            iconWithAttr { defaultIconAttribute
                             | title = ""
                             , icon = "clear"
                         }
        }
