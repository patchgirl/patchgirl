module RequestBuilderApp.RequestTree.App exposing (..)

--import RequestBuilderApp.RequestBuilder.App as RequestBuilder

import Animation
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Http
import Page exposing (..)
import Random
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation
import HttpError exposing(..)
import Element.Events exposing (..)
import RequestBuilderApp.RequestTree.Util exposing(..)


-- * model


type alias Model a =
    { a
        | requestCollection : RequestCollection
        , notification : Maybe Notification
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestId : Maybe Uuid
        , displayedRequestBuilderView : BuilderView Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = ToggleMenu (Maybe Uuid)
    | ToggleFolder Uuid
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu mId ->
            ({ model | displayedRequestNodeMenuId = mId }, Cmd.none)

        ToggleFolder id ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyRequestNode id toggleFolder) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * view


view : Model a -> Element Msg
view model =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection
    in
    column [ spacing 10 ] (nodeView model requestNodes)


nodeView : Model a -> List RequestNode -> List (Element Msg)
nodeView model requestCollection =
    case requestCollection of
        [] ->
            []

        node :: tail ->
            case node of
                Folder { id, name, open, children } ->
                    let
                        (Children c) =
                            children

                        folderChildrenView =
                            nodeView model c

                        tailView =
                            nodeView model tail

                        currentFolderView =
                            folderView id model name folderChildrenView open
                    in
                    currentFolderView :: tailView

                File { id, name } ->
                    let
                        tailView =
                            nodeView model tail

                        currentFileView =
                            fileView id model name
                    in
                    currentFileView :: tailView



-- ** file view


fileView : Uuid -> Model a -> Editable String -> Element Msg
fileView id model eName =
    let
        showMenu =
            model.displayedRequestNodeMenuId == Just id

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ReqPage (EditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none

        name =
            editedOrNotEditedValue eName

        selected =
            getBuilderId model.displayedRequestBuilderView == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular
    in
    row [ onMouseEnter (ToggleMenu (Just id))
        , onMouseLeave (ToggleMenu Nothing)
        ]
        [ el [ onRight folderMenuView ] <|
              link [ weight ]
                  { url = href (ReqPage (RunView id))
                  , label = el [] <| iconWithTextAndColor "label" name color
                  }
        ]


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model eName folderChildrenView open =
    let
        folderWithIconView : Element Msg
        folderWithIconView =
            let
                folderIconText =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            iconWithText folderIconText name

        showMenu =
            Just id == model.displayedRequestNodeMenuId

        name =
            editedOrNotEditedValue eName

        selected =
            getBuilderId model.displayedRequestBuilderView == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

        folderReadView : Bool -> Element Msg
        folderReadView isOpen =
            Input.button [ weight ]
                { onPress = Just <| ToggleFolder id
                , label = folderWithIconView
                }

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ReqPage (EditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none
    in
    column [ width (fill |> maximum 300) ]
        [ row [ onMouseEnter (ToggleMenu (Just id))
              , onMouseLeave (ToggleMenu Nothing)
              ]
              [ folderReadView open
              , folderMenuView
              ]
        , case open of
            True ->
                column [ spacing 10, paddingXY 20 10 ] folderChildrenView

            False ->
                none
        ]
