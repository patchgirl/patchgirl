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


type alias Node a b = NodeType { a | id : Uuid } { b | id : Uuid }

findNode : List (Node a b)
         -> ({ a | id : Uuid } -> List (Node a b))
         -> Uuid
         -> Maybe (Node a b)
findNode nodes getChildren id =
    let
        find : (Node a b) -> Maybe (Node a b)
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
                            findNode (getChildren folder) getChildren id
    in
    List.head <| catMaybes (List.map find nodes)


-- ** find request


findRequestNode : List RequestNode -> Uuid -> Maybe RequestNode
findRequestNode nodes id =
    let
        getChildren folder =
            let
                (RequestChildren children) =
                    folder.children
            in
            children
    in
    findNode nodes getChildren id


-- ** find scenario


findScenarioNode : List ScenarioNode -> Uuid -> Maybe ScenarioNode
findScenarioNode nodes id =
    let
        getChildren folder =
            let
                (ScenarioChildren children) =
                    folder.children
            in
            children
    in
    findNode nodes getChildren id


-- ** find pg


findPgNode : List PgNode -> Uuid -> Maybe PgNode
findPgNode nodes id =
    let
        getChildren folder =
            let
                (PgChildren children) =
                    folder.children
            in
            children
    in
    findNode nodes getChildren id



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


-- ** get node Id


getNodeId : Node a b -> Uuid
getNodeId requestNode =
    case requestNode of
        Folder { id } -> id
        File { id } -> id

-- ** modify


modifyPgNode : Uuid -> (PgNode -> PgNode) -> PgNode -> PgNode
modifyPgNode id f requestNode =
    case getNodeId requestNode == id of
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
    case getNodeId requestNode == id of
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

modifyScenarioNode : Uuid -> (ScenarioNode -> ScenarioNode) -> ScenarioNode -> ScenarioNode
modifyScenarioNode id f node =
    case getNodeId node == id of
        True ->
            f node

        False ->
            case node of
                File file ->
                    File file

                Folder folder ->
                    let
                        (ScenarioChildren children) =
                            folder.children
                    in
                    Folder
                        { folder
                            | children =
                                ScenarioChildren (List.map (modifyScenarioNode id f) children)
                        }

-- ** delete


deleteRequestNode : Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getNodeId requestNode == idToDelete of
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
    case getNodeId pgNode == idToDelete of
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


toggleFolder : NodeType { a | open : Bool } b -> NodeType { a | open : Bool } b
toggleFolder node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            Folder { folder | open = not folder.open }


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

mkdirScenario : Uuid -> ScenarioNode -> ScenarioNode
mkdirScenario id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (ScenarioChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = ScenarioChildren (mkDefaultScenarioFolder id :: children)
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

touchScenario : ScenarioNode -> ScenarioNode -> ScenarioNode
touchScenario newNode parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            let
                (ScenarioChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = ScenarioChildren (newNode :: children)
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


rename : String
       -> NodeType { a | name : Editable String } { b | name : Editable String }
       -> NodeType { a | name : Editable String } { b | name : Editable String }
rename newName node =
    case node of
        Folder folder ->
            Folder { folder | name = NotEdited newName }

        File file ->
            File { file | name = NotEdited newName }

tempRename : String
           -> NodeType { a | name : Editable String } { b | name : Editable String }
           -> NodeType { a | name : Editable String } { b | name : Editable String }
tempRename newName node =
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

mkDefaultScenarioFolder : Uuid -> ScenarioNode
mkDefaultScenarioFolder id =
    Folder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = ScenarioChildren []
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

mkDefaultScenarioFile : Uuid -> ScenarioNode
mkDefaultScenarioFile id =
    File
        { id = id
        , environmentId = NotEdited Nothing
        , name = NotEdited ""
        , scenes = []
        }


-- * view


-- ** environment selection


environmentSelectionView : List Environment -> Maybe Uuid -> (Uuid -> a) -> Element a
environmentSelectionView environments selectedEnvironmentToRunIndex msg =
    let
        entryView environment =
            Input.option environment.id (text (notEditedValue environment.name))

    in
    Input.radio [ padding 20, spacing 10 ]
        { onChange = msg
        , selected = selectedEnvironmentToRunIndex
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
