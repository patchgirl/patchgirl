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


findFile : List (Node file) -> Uuid -> Maybe (NodeRecord file)
findFile nodes id =
    case findNode nodes id of
        Just (File file) -> Just file
        _ -> Nothing

findFolder : List (Node file) -> Uuid -> Maybe (FolderRecord file)
findFolder nodes id =
    case findNode nodes id of
        Just (Folder folder) -> Just folder
        _ -> Nothing

findNode : List (Node file)
         -> Uuid
         -> Maybe (Node file)
findNode nodes id =
    let
        find : (Node file) -> Maybe (Node file)
        find node =
            case node of
                (File file) ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (Folder folder) ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            findNode folder.children id
    in
    List.head <| catMaybes (List.map find nodes)


-- ** get node Id and name


getNodeIdAndName : Node file
                 -> { id : Uuid
                    , name: Editable String
                    }
getNodeIdAndName node =
    case node of
        Folder { id, name } -> { id = id, name = name }
        File { id, name } -> { id = id, name = name }


-- ** modify


modifyNode : Uuid -> (Node file -> Node file) -> Node file -> Node file
modifyNode id f node =
    case (getNodeIdAndName node).id == id of
        True ->
            f node

        False ->
            case node of
                File file ->
                    File file

                Folder folder ->
                    Folder
                        { folder
                            | children =
                                List.map (modifyNode id f) folder.children
                        }

-- ** delete


deleteNode : Uuid -> Node file -> List (Node file)
deleteNode idToDelete node =
    case (getNodeIdAndName node).id == idToDelete of
        True ->
            []

        False ->
            case node of
                File requestFile ->
                    [ File requestFile ]

                Folder folder ->
                    [ Folder
                        { folder
                            | children = List.concatMap (deleteNode idToDelete) folder.children
                        }
                    ]


-- ** toggle


toggleFolder : Node file -> Node file
toggleFolder node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder { folder | open = not folder.open }


-- ** mkdir


mkdirNode : FolderRecord file -> Node file -> Node file
mkdirNode record node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder
                { folder
                    | children = (Folder record) :: folder.children
                    , open = True
                }


-- ** touch


touchNode : Node file -> Node file -> Node file
touchNode newNode parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            Folder
                { folder
                    | children = newNode :: folder.children
                    , open = True
                }


-- ** rename


rename : String
       -> Node file
       -> Node file
rename newName node =
    case node of
        Folder folder ->
            Folder { folder | name = NotEdited newName }

        File file ->
            File { file | name = NotEdited newName }

tempRename : String
           -> Node file
           -> Node file
tempRename newName node =
    case node of
        Folder folder ->
            Folder { folder | name = changeEditedValue newName folder.name }

        File file ->
            File { file | name = changeEditedValue newName file.name }


-- ** mk default


mkDefaultFolder : Uuid -> String -> FolderRecord file
mkDefaultFolder id name =
    { id = id
    , name = NotEdited name
    , open = False
    , children = []
    }


-- *** request



mkDefaultRequestFile : Uuid -> String -> NodeRecord RequestFileRecord
mkDefaultRequestFile id name =
    { id = id
    , name = NotEdited name
    , httpUrl = NotEdited ""
    , httpMethod = NotEdited HttpGet
    , httpHeaders = NotEdited []
    , httpBody = NotEdited ""
    , showResponseView = False
    , whichResponseView = BodyResponseView
    , requestComputationResult = Nothing
    , runRequestIconAnimation = Animation.style []
    }


-- *** pg


mkDefaultPgFile : Uuid -> String -> NodeRecord PgFileRecord
mkDefaultPgFile id name =
    { id = id
    , name = NotEdited name
    , dbHost = NotEdited ""
    , dbPassword = NotEdited ""
    , dbPort = NotEdited ""
    , dbUser = NotEdited ""
    , dbName = NotEdited ""
    , sql = NotEdited ""
    , pgComputationOutput = Nothing
    , showResponseView = False
    }


-- *** scenario


mkDefaultScenarioFile : Uuid -> String -> NodeRecord ScenarioFileRecord
mkDefaultScenarioFile id name =
    { id = id
    , name = NotEdited name
    , environmentId = NotEdited Nothing
    , scenes = []
    }


-- * view


-- ** environment selection


environmentSelectionView : List Environment -> Maybe Uuid -> (Uuid -> a) -> Element a
environmentSelectionView environments selectedEnvironmentToRunId msg =
    let
        entryView environment =
            Input.option environment.id (text (notEditedValue environment.name))

    in
    Input.radio [ padding 20, spacing 10 ]
        { onChange = msg
        , selected = selectedEnvironmentToRunId
        , label = Input.labelAbove [] (text "Environment:")
        , options =
              List.map entryView environments
        }


-- ** close builder


closeBuilderView : Page -> Element a
closeBuilderView page =
    let
        exitLink =
            case page of
                ReqPage _ ->
                    href (ReqPage (LandingView DefaultView))
                PgPage _ ->
                    href (PgPage (LandingView DefaultView))
                EnvPage _ ->
                    href (EnvPage (LandingView DefaultView))
                _ ->
                    href (ScenarioPage (RichLandingView DefaultView))

    in
    link []
        { url = exitLink
        , label =
            iconWithAttr { defaultIconAttribute
                             | title = ""
                             , icon = "clear"
                         }
        }


-- ** pick folder view


folderTreeView : List RequestNode -> Maybe Uuid -> msg -> (Uuid -> msg) -> Element msg
folderTreeView nodes mParentFolderId selectRootFolderMsg selectFolderMsg =
    column [ spacing 10 ]
        [ text "Select a folder:"
        , rootFolderView mParentFolderId (NotEdited "/") selectRootFolderMsg (nodeView mParentFolderId selectFolderMsg nodes)
        ]

nodeView : Maybe Uuid -> (Uuid -> b) -> List RequestNode -> List (Element b)
nodeView mParentFolderId selectFolderMsg nodes =
    case nodes of
        [] ->
            []

        node :: tail ->
            case node of
                File _ -> nodeView mParentFolderId selectFolderMsg tail
                Folder { id, name, children } ->
                    let
                        folderChildrenView =
                            nodeView mParentFolderId selectFolderMsg children

                        tailView =
                            nodeView mParentFolderId selectFolderMsg tail

                        currentFolderView =
                            folderView id mParentFolderId name selectFolderMsg folderChildrenView
                    in
                    currentFolderView :: tailView


rootFolderView : Maybe Uuid -> Editable String -> b -> List (Element b) -> Element b
rootFolderView mParentFolderId eName selectRootFolderMsg folderChildrenView =
    let
        selected =
            mParentFolderId == Nothing
    in
    fView selected eName selectRootFolderMsg folderChildrenView

folderView : Uuid -> Maybe Uuid -> Editable String -> (Uuid -> b) -> List (Element b) -> Element b
folderView id mParentFolderId eName selectFolderMsg folderChildrenView =
    let
        selected =
            mParentFolderId == Just id
    in
    fView selected eName (selectFolderMsg id) folderChildrenView

fView : Bool -> Editable String -> a -> List (Element a) -> Element a
fView selected eName selectFolderMsg folderChildrenView =
    let
        label : String -> Element a
        label title =
            iconWithAttr { defaultIconAttribute
                             | title = title
                             , iconSize = Nothing
                             , icon = "folder"
                             , iconVerticalAlign = Just "bottom"
                             , primIconColor =
                               case selected of
                                   True -> Just primaryColor
                                   False -> Nothing
                         }

        selectedAttributes =
            case selected of
                False -> []
                True -> [ Font.bold ]

        selectFolderBtn : Element a
        selectFolderBtn =
            Input.button selectedAttributes
                { onPress = Just selectFolderMsg
                , label = label (editedOrNotEditedValue eName)
                }
    in
    column [ spacing 10 ]
        [ selectFolderBtn
        , column [ paddingXY 20 0 ] folderChildrenView
        ]


-- ** pick folder view 2


pgFolderTreeView : List PgNode -> Maybe Uuid -> msg -> (Uuid -> msg) -> Element msg
pgFolderTreeView nodes mParentFolderId selectRootFolderMsg selectFolderMsg =
    column [ spacing 10 ]
        [ text "Select a folder:"
        , rootFolderView mParentFolderId (NotEdited "/") selectRootFolderMsg (pgNodeView mParentFolderId selectFolderMsg nodes)
        ]

pgNodeView : Maybe Uuid -> (Uuid -> b) -> List PgNode -> List (Element b)
pgNodeView mParentFolderId selectFolderMsg nodes =
    case nodes of
        [] ->
            []

        node :: tail ->
            case node of
                File _ -> pgNodeView mParentFolderId selectFolderMsg tail
                Folder { id, name, children } ->
                    let
                        folderChildrenView =
                            pgNodeView mParentFolderId selectFolderMsg children

                        tailView =
                            pgNodeView mParentFolderId selectFolderMsg tail

                        currentFolderView =
                            folderView id mParentFolderId name selectFolderMsg folderChildrenView
                    in
                    currentFolderView :: tailView
