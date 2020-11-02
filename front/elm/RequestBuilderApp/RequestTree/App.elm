module RequestBuilderApp.RequestTree.App exposing (..)

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
import BuilderUtil exposing (..)


-- * model


type alias Model a =
    { a
        | requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestBuilderView : BuilderView Uuid
    }


-- * message


type Msg
    = ToggleMenu (Maybe Uuid)
    | ToggleFolder Uuid


-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ToggleMenu mId ->
            { model | displayedRequestNodeMenuId = mId }

        ToggleFolder id ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyNode id toggleFolder) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }
            in
            newModel


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
                        folderChildrenView =
                            nodeView model children

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

        name =
            notEditedValue eName

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
    el [ onMouseEnter (ToggleMenu (Just id))
       , onMouseLeave (ToggleMenu Nothing)
       ] <|
        row [ weight ] <|
            [ case showMenu of
                  True ->
                      link []
                          { url = href (ReqPage (EditView (DefaultEditView id)))
                          , label =
                              iconWithAttr { defaultIconAttribute
                                               | title = ""
                                               , icon = "edit"
                                           }
                          }

                  False ->
                      iconWithAttr { defaultIconAttribute
                                       | title = ""
                                       , primIconColor = Just color
                                       , icon = "label"
                                   }
            , link [ weight ]
                { url = href (ReqPage (RunView id))
                , label = text name
                }
            ]


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model eName folderChildrenView open =
    let
        showMenu =
            Just id == model.displayedRequestNodeMenuId

        name =
            notEditedValue eName

        selected =
            getBuilderId model.displayedRequestBuilderView == Just id

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

    in
    column []
        [ row [ weight
              , onMouseEnter (ToggleMenu (Just id))
              , onMouseLeave (ToggleMenu Nothing)
              ]
              [ link []
                    { url = href (ReqPage (EditView (DefaultEditView id)))
                    , label =
                        iconWithAttr { defaultIconAttribute
                                         | title = ""
                                         , icon =
                                           case showMenu of
                                               True -> "edit"
                                               False ->
                                                   case open of
                                                       False -> "keyboard_arrow_right"
                                                       True -> "keyboard_arrow_down"
                                     }
                    }
              , Input.button [ weight ]
                  { onPress = Just <| ToggleFolder id
                  , label = text name
                  }
              ]
        , case open of
              True ->
                  column [ spacing 10, paddingXY 20 10 ] folderChildrenView

              False ->
                  none
        ]
